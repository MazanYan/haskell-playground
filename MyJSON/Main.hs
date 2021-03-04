module Main (main) where

import MyJSON

myObject = JObject [
        (
            "Kakashi", 
            JObject [
                ("age", JInt 27),
                ("position", JString "jonin"),
                ("team", JInt 7),
                (
                    "subordinates",
                    JArray [
                        JString "Sasuke",
                        JString "Sakura",
                        JString "Naruto"
                    ]
                )
            ]
        )
    ]

main = do
    print (show myObject)
