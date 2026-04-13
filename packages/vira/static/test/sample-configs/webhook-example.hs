\ctx pipeline ->
  pipeline
    { postBuild.webhooks =
        [ webhook GET "https://example.com/notify?branch=$VIRA_BRANCH&commit=$VIRA_COMMIT_ID" [] Nothing
        ]
    }
