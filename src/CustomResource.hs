-- | http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-resource-cfn-customresource.html
module CustomResource
  ( Config
  , CustomResource
  , context
  , customResource
  , customResourceProperties
  , customResourceServiceToken
  , lambdaFunctionArn
  , resources
  )
where

import CustomResource.Prelude
import Stratosphere
import Stratosphere.Helpers
import Stratosphere.ResourceImports

import qualified Data.Aeson          as JSON
import qualified Data.HashMap.Strict as HashMap
import qualified Data.Vector         as Vector

-- | Full data type definition for CloudFormationCustomResource.
--
-- See 'customResource' for a more convenient constructor.
data CustomResource =
  CustomResource
  { _CustomResourceType         :: Text
  , _CustomResourceServiceToken :: Val Text
  , _CustomResourceProperties   :: Map Text Value
  } deriving stock (Show, Eq)

instance ToResourceProperties CustomResource where
  toResourceProperties CustomResource{..} =
    ResourceProperties
    { resourcePropertiesType = "Custom::" <> _CustomResourceType
    , resourcePropertiesProperties =
        hashMapFromList
          $ ("ServiceToken", toJSON _CustomResourceServiceToken)
          : toList _CustomResourceProperties
    }

-- | Constructor for 'CustomResource' containing required fields as arguments.
customResource
  :: Text     -- ^ 'customResourceType'
  -> Val Text -- ^ 'customResourceServiceToken'
  -> CustomResource
customResource _CustomResourceType _CustomResourceServiceToken =
  CustomResource
    { _CustomResourceProperties = []
    , ..
    }

-- | http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-resource-cfn-customresource.html#cfn-customresource-servicetoken
customResourceServiceToken :: Lens' CustomResource (Val Text)
customResourceServiceToken =
  lens
    _CustomResourceServiceToken
    (\record value -> record { _CustomResourceServiceToken = value })

-- | http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-resource-cfn-customresource.html#cfn-customresource-servicetoken
customResourceProperties :: Lens' CustomResource (Map Text Value)
customResourceProperties =
  lens
    _CustomResourceProperties
    (\record value -> record { _CustomResourceProperties = value })

data Config = Config
  { lambdaFunctionArn :: Val Text
  , resources         :: Resources
  }

context :: LambdaFunctionCode -> [Text] -> Config
context code allowedActions = Config{..}
  where
    resources :: Resources
    resources = [function, logGroup, role]

    role = mkLambdaFunctionRole allowedActions

    logGroup = mkLambdaLogGroup logicalName physicalName

    lambdaFunctionArn = getAttArn function

    function
      = resource logicalName
      $ lambdaFunction
        code
        "function.handler"
        (getAttArn role)
        (Literal $ OtherRuntime "provided")
      & lfFunctionName ?~ physicalName
      & lfTimeout      ?~ Literal 300

logicalName :: Text
logicalName = "CustomResourceLambda"

physicalName :: Val Text
physicalName = mkName (Literal logicalName)

mkLambdaFunctionRole :: [Text] -> Resource
mkLambdaFunctionRole allowedActions
  = resource "CustomResourceLambdaFunctionRole"
  $ iamRole lambdaAssumeRole
  & iamrPolicies ?~ [cloudwatchPolicy, resourcePolicy]
  where
    resourcePolicy = allowResourcePolicy
      (JSON.String "*")
      "custom-resources"
      allowedActions

    cloudwatchPolicy = cloudwatchLambdaPolicy physicalName

lambdaAssumeRole :: Object
lambdaAssumeRole = HashMap.fromList
  [ ("Version", String "2012-10-17")
  , ("Statement"
    , object
      [ ("Action", "sts:AssumeRole")
      , ("Effect", "Allow")
      , ("Principal", object [("Service", "lambda.amazonaws.com")])
      ]
    )
  ]

mkLambdaLogGroup :: Text -> Val Text -> Resource
mkLambdaLogGroup lambdaLogicalName lambdaPhysicalName
  = resource
    (lambdaLogicalName <> "LogGroup")
  $ logsLogGroup
  & llgLogGroupName    ?~ lambdaLogGroupName lambdaPhysicalName
  & llgRetentionInDays ?~ Literal 14

lambdaLogGroupName :: Val Text -> Val Text
lambdaLogGroupName lambdaName =
  Join "/" ["/aws/lambda", lambdaName]

allowResourcePolicy :: ToJSON a => a -> Text -> [Text] -> IAMRolePolicy
allowResourcePolicy resources name actions
  = iamRolePolicy (HashMap.singleton "Statement" statement) (Literal name)
  where
    statement :: Value
    statement = object
      [ ("Action",   Array $ Vector.fromList (toJSON <$> actions))
      , ("Effect",   "Allow")
      , ("Resource", toJSON resources)
      ]

cloudwatchLambdaPolicy :: Val Text -> IAMRolePolicy
cloudwatchLambdaPolicy lambdaName =
  allowResourcePolicy
    logStreamsArn
    "cloudwatch-logs"
    [ "logs:CreateLogStream"
    , "logs:PutLogEvents"
    ]
  where
    logStreamsArn =
      Join
        ":"
        [ "arn"
        , "aws"
        , "logs"
        , region
        , accountId
        , "log-group"
        , lambdaLogGroupName lambdaName
        , "*"
        ]

mkName :: Val Text -> Val Text
mkName name = Join "-" [stackName, name]

accountId :: Val Text
accountId = Ref "AWS::AccountId"

region :: Val Text
region = Ref "AWS::Region"

stackName :: Val Text
stackName = Ref "AWS::StackName"

getAttArn :: Resource -> Val Text
getAttArn item = GetAtt (itemName item) "Arn"
