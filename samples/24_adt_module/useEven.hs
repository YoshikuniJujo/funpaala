import Even

evenAs :: Even -> String
evenAs e = replicate (fromIntegral $ fromEven e) 'a'
