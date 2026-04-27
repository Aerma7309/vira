\ctx pipeline ->
  pipeline
    { hooks.onSuccess = Just (fromString "notify-jenkins")
    }
