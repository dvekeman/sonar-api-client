# Sonar API Client

This package is an example of a Sonar (5.6) API client written in haskell using servant.

To build and execute

```
stack build
stack exec sonar-api-client-exe -- "sonar.mydomain.com"
```

Currently only a part of the profiles and rules API is implemented
```
type ProfileApi =
    -- PROFILES /api/profiles?langauge=<..>&name=<..>
        "api" :> "profiles" :> 
                    QueryParam "language" T.Text :>
                    QueryParam "name" T.Text :> 
                    Get '[JSON] [MP.Profile]
    -- RULES    /api/rules/show?key=<..>
    :<|> "api" :> "rules" :> "show" :> 
        QueryParam "key" T.Text :>
        Get '[JSON] MR.Rule
```

There are two formatters

- CSV
- Markdown

Currently it's pretty basic

For **Markdown** 

```
main :: IO ()
main = do 
  rules <- fetchProfileRules
  let mdRules = map toMDRule rules
  MD.writeRules mdRules
```

For **CSV**

```
main :: IO ()
main = do 
  rules <- fetchProfileRules
  let csvRules = map toCsvRule rules
  Csv.writeRules csvRules 
```

## Todo

- Split up the 'client' and the example.
- Complete the client API code
- Include sonar version in the API client package name 