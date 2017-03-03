promptAndCheckId :: [C.CourseID] -> IO C.CourseID
promptAndCheckId lst = do
    putStrLn "Which course to sync the files (ID):"
    eitherIdInputed <- readEither <$> getLine
    let idValid = either (const Nothing) (\i -> if i `elem` lst then Just i else Nothing)
                  eitherIdInputed
    maybe (putStrLn "Invalid id." >> promptAndCheckId lst) return idValid

promptAndCheckPath :: IO FilePath
promptAndCheckPath = do
    path <- getCurrentDirectory
    putStrLn $ "Presently at path " ++ path
    putStrLn "Download to where (relative/absolute):"
    pathInputed <- getLine
    let absolute = if isRelative pathInputed then path </> pathInputed
                                             else pathInputed
    exist <- doesDirectoryExist absolute
    unless exist $ do
        putStrLn "Path does not exist; creating..."
        createDirectory absolute
    putStrLn $ "Will download to " ++ absolute
    return absolute

printCourse :: C.Course -> String
printCourse course = show (C.name course) ++ "\t\t\t\t" ++ show (C.id course) ++ "\n"
