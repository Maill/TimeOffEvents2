module Views

open Giraffe.GiraffeViewEngine

let indexView = 
    html [] [
        head [] [
            meta [ _charset "UTF-8"; ]
            title [] [ rawText("TimeOffEvents - Logon") ]
            link [ _rel "stylesheet"; _href "./css/common.css"; _type "text/css"; ]
            link [ _rel "stylesheet"; _href "./css/index.css"; _type "text/css"; ]
        ]
        body [] [
            div [ _id "informationContainer"; _class "informationContainer" ] []
            form [ _class "logonForm"; _id "logonForm" ] [
                div [ _class "logonFormTitle" ] [ rawText("Time Off Manager") ]
                div [ _class "logonFormSubtitle" ] [ rawText("Logon Form") ]
                div [ _class "logonFormLabel" ] [ rawText("Identifier") ]
                input [ _id "usernameInput"; _class "logonFormInput"; _type "text"; _placeholder "Enter your identifier"; _required ]
                div [ _class "logonFormLabel" ] [ rawText("Password") ]
                input [ _id "passwordInput"; _class "logonFormInput"; _type "password"; _placeholder "Enter your password"; _required ]
                button [ _class "logonFormSubmit"; _type "submit" ] [ rawText("Submit") ]
                script [ _src "./js/jQuery.js" ] []
                script [ _src "./js/informationBlock.js" ] []
                script [ _src "./js/logonForm.js" ] []
            ]
        ]
    ]

let homeView =
    html [] [
        head [] [
            meta [ _charset "UTF-8"; ]
            title [] [ rawText("TimeOffEvents - Logon") ]
            link [ _rel "stylesheet"; _href "./css/common.css"; _type "text/css"; ]
        ]
        body [] [
            div [ _id "informationContainer"; _class "informationContainer" ] []
            div [] [ rawText("Welcome !") ]
        ]
    ]

let lostView =
    html [] [
        head [] [
            meta [ _charset "UTF-8"; ]
            title [] [ rawText("TimeOffEvents - Logon") ]
            link [ _rel "stylesheet"; _href "./css/common.css"; _type "text/css"; ]
            link [ _rel "stylesheet"; _href "./css/lost.css"; _type "text/css"; ]
        ]
        body [] [
            div [ _class "lostBlock" ] [
                div [ _class "lostBlockTitle" ] [ rawText("404 - not found") ]
                div [ _class "lostBlockMessage" ] [ rawText("The page you are looking for does not exist") ]
                a [ _href "/"; _class "lostBlockLink" ] [ rawText("Return home") ]
            ]
        ]
    ]