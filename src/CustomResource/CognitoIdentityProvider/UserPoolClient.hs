module CustomResource.CognitoIdentityProvider.UserPoolClient (requestHandler) where

import CustomResource.AWS
import CustomResource.Lambda
import CustomResource.Prelude
import Data.Aeson ((.!=), (.:), (.:?))
import GHC.Generics (Generic)
import Network.AWS.CognitoIdentityProvider.CreateUserPoolClient
import Network.AWS.CognitoIdentityProvider.DeleteUserPoolClient
import Network.AWS.CognitoIdentityProvider.Types
import Network.AWS.CognitoIdentityProvider.UpdateUserPoolClient
import Numeric.Natural (Natural)

import qualified Data.Aeson              as JSON
import qualified Data.Aeson.Text         as JSON
import qualified Data.Aeson.Types        as JSON
import qualified Data.Map.Strict         as Map
import qualified Data.Text.Lazy          as Text
import qualified Data.Text.Lazy.Encoding as Text

data UserPoolClient :: State -> * where
  UserPoolClient ::
    { allowedOAuthFlowsUserPoolClient :: Maybe Bool
    , allowedOAuthScopes              :: [Text]
    , analyticsConfiguration          :: Maybe AnalyticsConfigurationType
    , callbackURLs                    :: [Text]
    , clientName                      :: Text
    , defaultRedirectURI              :: Maybe Text
    , explicitAuthFlows               :: [ExplicitAuthFlowsType]
    , generateSecret                  :: Maybe Bool
    , logoutURLs                      :: [Text]
    , readAttributes                  :: [Text]
    , refreshTokenValidility          :: Maybe Natural
    , supportedIdentityProviders      :: [Text]
    , userPoolId                      :: Text
    , writeAttributes                 :: [Text]
    } -> UserPoolClient a
  deriving stock Generic

instance JSON.FromJSON (UserPoolClient a) where
  parseJSON = JSON.withObject "UserPoolClient" $ \object -> do
    allowedOAuthFlowsUserPoolClient <-
      JSON.explicitParseFieldMaybe
        parseTextBool
        object
        "AllowedOAuthFlowsUserPoolClient"
    allowedOAuthScopes <-
      object .:? "AllowedOAuthScopes" .!= empty
    analyticsConfiguration <-
      object .:? "AnalyticsConfiguration"
    callbackURLs <-
      object .:? "CallbackURLs" .!= empty
    clientName <-
      object .: "ClientName"
    defaultRedirectURI <-
      object .:? "DefaultRedirectURI"
    explicitAuthFlows <-
      object .:? "ExplicitAuthFlows" .!= empty
    generateSecret <-
      JSON.explicitParseFieldMaybe
        parseTextBool
        object
        "GenerateSecret"
    logoutURLs <-
      object .:? "LogoutURLs" .!= empty
    readAttributes <-
      object .:? "ReadAttributes" .!= empty
    refreshTokenValidility <-
      object .:? "RefreshTokenValidility"
    supportedIdentityProviders <-
      object .:? "SupportedIdentityProviders" .!= empty
    userPoolId <-
      object .: "UserPoolId"
    writeAttributes <-
      object .:? "WriteAttributes" .!= empty
    pure $ UserPoolClient{..}

data ResourceId = ResourceId
  { clientId   :: Text
  , userPoolId :: Text
  }
  deriving anyclass (JSON.FromJSON, JSON.ToJSON)
  deriving stock Generic

instance ToPhysicalResourceId ResourceId where
  toPhysicalResourceId
    = PhysicalResourceId
    . Text.toStrict
    . JSON.encodeToLazyText

instance FromPhysicalResourceId ResourceId where
  fromPhysicalResourceId (PhysicalResourceId text) =
    JSON.eitherDecode' (Text.encodeUtf8 $ Text.fromStrict text)


requestHandler :: RequestHandler
requestHandler = mkRequestHandler resourceType ResourceHandler{..}
  where
    resourceType = mkResourceType "CognitoIdentityProviderUserPoolClient"

    createResource
      :: RequestMetadata
      -> UserPoolClient 'New -> AWS Response
    createResource metadata UserPoolClient{..}
      = fromRequest metadata mkPhysicalResourceId (mkResponseData <=< view cupcrsUserPoolClient)
      $ createUserPoolClient userPoolId clientName
      & cupcAllowedOAuthFlowsUserPoolClient .~ allowedOAuthFlowsUserPoolClient
      & cupcAllowedOAuthScopes              .~ allowedOAuthScopes
      & cupcAnalyticsConfiguration          .~ analyticsConfiguration
      & cupcCallbackURLs                    .~ callbackURLs
      & cupcDefaultRedirectURI              .~ defaultRedirectURI
      & cupcExplicitAuthFlows               .~ explicitAuthFlows
      & cupcGenerateSecret                  .~ generateSecret
      & cupcLogoutURLs                      .~ logoutURLs
      & cupcReadAttributes                  .~ readAttributes
      & cupcRefreshTokenValidity            .~ refreshTokenValidility
      & cupcSupportedIdentityProviders      .~ supportedIdentityProviders
      & cupcWriteAttributes                 .~ writeAttributes

    mkPhysicalResourceId response =
      toPhysicalResourceId <$> (fromClient =<< view cupcrsUserPoolClient response)

      where
        fromClient :: UserPoolClientType -> Maybe ResourceId
        fromClient client
          = tryCreate (client ^. upctUserPoolId) (client ^. upctClientId)

        tryCreate :: Maybe Text -> Maybe Text -> Maybe ResourceId
        tryCreate (Just userPoolId) (Just clientId) = pure ResourceId{..}
        tryCreate _                 _               = empty

    deleteResource
      :: RequestMetadata
      -> ResourceId
      -> AWS Response
    deleteResource metadata resourceId@ResourceId{..} =
      fromRequest
        metadata
        (const . pure $ toPhysicalResourceId resourceId)
        (const $ pure emptyResponseData)
        (deleteUserPoolClient userPoolId clientId)

    updateResource
      :: RequestMetadata
      -> ResourceId
      -> UserPoolClient 'New
      -> UserPoolClient 'Old
      -> AWS Response
    updateResource
      metadata
      resourceId@(ResourceId resourceUserPoolId resourceClientName)
      UserPoolClient{..}
      _oldProperties
        = require (resourceUserPoolId == userPoolId) "Updates to user pool id are not supported"
        . fromRequest metadata (const . pure $ toPhysicalResourceId resourceId) (mkResponseData <=< view uupcrsUserPoolClient)
        $ updateUserPoolClient resourceUserPoolId resourceClientName
        & uupcAllowedOAuthFlowsUserPoolClient .~ allowedOAuthFlowsUserPoolClient
        & uupcAllowedOAuthScopes              .~ allowedOAuthScopes
        & uupcAnalyticsConfiguration          .~ analyticsConfiguration
        & uupcCallbackURLs                    .~ callbackURLs
        & uupcDefaultRedirectURI              .~ defaultRedirectURI
        & uupcExplicitAuthFlows               .~ explicitAuthFlows
        & uupcLogoutURLs                      .~ logoutURLs
        & uupcReadAttributes                  .~ readAttributes
        & uupcRefreshTokenValidity            .~ refreshTokenValidility
        & uupcSupportedIdentityProviders      .~ supportedIdentityProviders
        & uupcWriteAttributes                 .~ writeAttributes
      where
        require bool message action = check metadata resourceId action message bool

mkResponseData :: UserPoolClientType -> Maybe ResponseData
mkResponseData client = ResponseData . Map.singleton "Id" <$> client ^. upctClientId

parseTextBool :: JSON.Value -> JSON.Parser Bool
parseTextBool = JSON.withText "text encoded bool" $ \case
  "true"  -> pure True
  "false" -> pure False
  other   -> fail $ "unexpected: " <> show other
