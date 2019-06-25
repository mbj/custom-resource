{-# LANGUAGE RankNTypes #-}

module CustomResource.CognitoIdentityProvider.UserPoolClient (requestHandler) where

import Control.Lens (Lens')
import CustomResource.AWS
import CustomResource.Lambda
import CustomResource.Prelude
import Data.Aeson ((.!=), (.:), (.:?))
import Data.Map.Strict (Map)
import GHC.Generics (Generic)
import Network.AWS.CognitoIdentityProvider.CreateUserPoolClient
import Network.AWS.CognitoIdentityProvider.DeleteUserPoolClient
import Network.AWS.CognitoIdentityProvider.Types
import Network.AWS.CognitoIdentityProvider.UpdateUserPoolClient
import Network.AWS.Types (Rs)
import Numeric.Natural (Natural)
import UnliftIO.Exception (tryAny)

import qualified Data.Aeson              as JSON
import qualified Data.Aeson.Text         as JSON
import qualified Data.Aeson.Types        as JSON
import qualified Data.Map.Strict         as Map
import qualified Data.Text.Lazy          as Text
import qualified Data.Text.Lazy.Encoding as Text

data UserPoolClient :: State -> * where
  UserPoolClient ::
    { allowedOAuthFlows               :: [OAuthFlowType]
    , allowedOAuthFlowsUserPoolClient :: Maybe Bool
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
    allowedOAuthFlows <-
      object .:? "AllowedOAuthFlows" .!= empty
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
      = fromAWSRequest unknownPhysicalResourceId metadata (mkUserPoolResponse metadata cupcrsUserPoolClient)
      $ createUserPoolClient userPoolId clientName
      & cupcAllowedOAuthFlows               .~ allowedOAuthFlows
      & cupcAllowedOAuthFlowsUserPoolClient .~ allowedOAuthFlowsUserPoolClient
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

    deleteResource
      :: RequestMetadata
      -> ResourceId
      -> AWS Response
    deleteResource metadata resourceId@ResourceId{..} =
      fromAWSRequest
        resourceId
        metadata
        (const . Right $ mkSuccessResponse metadata resourceId)
        (deleteUserPoolClient userPoolId clientId)

    updateResource
      :: RequestMetadata
      -> ResourceId
      -> UserPoolClient 'New
      -> UserPoolClient 'Old
      -> AWS Response
    updateResource
      metadata
      resourceId@(ResourceId resourceClientId resourceUserPoolId)
      newProperties@UserPoolClient{..}
      _oldProperties
        = if resourceUserPoolId == userPoolId
            then performUpdate
            else createResource metadata newProperties
      where
        performUpdate
          = fromAWSRequest resourceId metadata (mkUserPoolResponse metadata uupcrsUserPoolClient)
          $ updateUserPoolClient resourceUserPoolId resourceClientId
          & uupcAllowedOAuthFlows               .~ allowedOAuthFlows
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

mkUserPoolResponse
  :: RequestMetadata
  -> Lens' a (Maybe UserPoolClientType)
  -> a
  -> Either Text Response
mkUserPoolResponse metadata lens response = do
  userPoolClient <- try "No user pool client" $ view lens response
  clientId       <- try "No client id"        $ userPoolClient ^. upctClientId
  userPoolId     <- try "No user pool id"     $ userPoolClient ^. upctUserPoolId

  Right $
    mkResponse
      metadata
      SUCCESS
      ResourceId{..}
      (ResponseData . addSecret (userPoolClient ^. upctClientSecret) $ Map.singleton "Id" clientId)
      NoEcho
      (ResponseReason "SUCCESS")

  where
    try :: Text -> Maybe a -> Either Text a
    try message = maybe (Left message) Right

    addSecret :: Maybe Text -> Map Text Text -> Map Text Text
    addSecret = maybe identity (Map.insert "ClientSecret")

fromAWSRequest
  :: forall a b . (AWSRequest a, ToPhysicalResourceId b)
  => b
  -> RequestMetadata
  -> (Rs a -> Either Text Response)
  -> a
  -> AWS Response
fromAWSRequest resourceId metadata tryResponse awsRequest =
  either
    (mkFailure . convertText . show)
    fromResponse =<< tryAny (send awsRequest)
  where
    fromResponse :: Rs a -> AWS Response
    fromResponse = either mkFailure pure . tryResponse

    mkFailure = mkFailedResponse metadata resourceId

parseTextBool :: JSON.Value -> JSON.Parser Bool
parseTextBool = JSON.withText "text encoded bool" $ \case
  "true"  -> pure True
  "false" -> pure False
  other   -> fail $ "unexpected: " <> show other
