module Scaffolding.TimeExtra where

import Data.Time.Clock

fuzzyDiffTime :: NominalDiffTime -> String
fuzzyDiffTime ntd =
    let seconds = truncate ntd :: Integer
        minutes = seconds `div` 60
        hours   = minutes `div` 60
        days    = hours `div` 24
        weeks   = days `div` 7
        years   = weeks `div` 52 -- not quite accurate?
    in
      case years of
        1 -> "1 year ago"
        n | n > 1 -> show years ++ " years ago"
        _ -> 
            case weeks of
              1 -> "1 week ago"
              n | n > 1 -> show weeks ++ " weeks ago"
              _ -> 
                  case days of
                    1 -> "yesterday"
                    n | n > 1 -> show days ++ " days ago"
                    _ -> 
                        case hours of
                          1 -> "1 hour ago"
                          n | n > 1 -> show hours ++ " hours ago"
                          _ -> 
                              case minutes of
                                1 -> "1 minute ago"
                                n | n > 1 -> show minutes ++ " minutes ago"
                                _ -> 
                                    case seconds of
                                      1 -> "1 second ago"
                                      _ -> show seconds ++ " seconds"
