-- (c) 2007 Andy Gill
-- Main driver for Hpc

module Trace.Haptics.Main where

import Data.Version
import Trace.Haptics.Combine
import Trace.Haptics.Draft
import Trace.Haptics.Flags
import Trace.Haptics.Markup
import Trace.Haptics.Overlay
import Trace.Haptics.Report
import Trace.Haptics.ShowTix
import Paths_haptics
import System.Console.GetOpt
import System.Environment
import System.Exit

helpList :: IO ()
helpList =
  putStrLn $
    "Usage: hpc COMMAND ...\n\n"
      ++ section "Commands" help
      ++ section "Reporting Coverage" reporting
      ++ section "Processing Coverage files" processing
      ++ section "Coverage Overlays" overlays
      ++ section "Others" other
      ++ ""
  where
    help = ["help"]
    reporting = ["report", "markup"]
    overlays = ["overlay", "draft"]
    processing = ["sum", "combine", "map"]
    other =
      [ name hook
      | hook <- hooks
      , name hook
          `notElem` concat [help, reporting, processing, overlays]
      ]

section :: String -> [String] -> String
section _ [] = ""
section msg cmds =
  msg ++ ":\n"
    ++ unlines
      [ take 14 ("  " ++ cmd ++ repeat ' ') ++ summary hook
      | cmd <- cmds
      , hook <- hooks
      , name hook == cmd
      ]

dispatch :: [String] -> IO ()
dispatch [] = do
  helpList
  exitSuccess
dispatch (txt : args0) =
  case lookup txt hooks' of
    Just plugin -> parse plugin args0
    _ -> parse help_plugin (txt : args0)
  where
    parse plugin args =
      case getOpt Permute (options plugin []) args of
        (_, _, errs) | not (null errs) ->
          do
            putStrLn "hpc failed:"
            sequence_
              [ putStr ("  " ++ err)
              | err <- errs
              ]
            putStrLn "\n"
            command_usage plugin
            exitFailure
        (o, ns, _) -> do
          let flags =
                final_flags plugin $
                  foldr ($) (
                    init_flags plugin) o
          implementation plugin flags ns

main :: IO ()
main = getArgs >>= dispatch

hooks :: [Plugin]
hooks =
  [ help_plugin
  , report_plugin
  , markup_plugin
  , sum_plugin
  , combine_plugin
  , map_plugin
  , showtix_plugin
  , overlay_plugin
  , draft_plugin
  , version_plugin
  ]

hooks' :: [(String, Plugin)]
hooks' = [(name hook, hook) | hook <- hooks]


help_plugin :: Plugin
help_plugin =
  Plugin
    { name = "help"
    , usage = "[<HPC_COMMAND>]"
    , summary = "Display help for hpc or a single command"
    , options = help_options
    , implementation = help_main
    , init_flags = default_flags
    , final_flags = default_final_flags
    }

help_main :: Flags -> [String] -> IO ()
help_main _ [] = do
  helpList
  exitSuccess
help_main _ (sub_txt : _) =
  case lookup sub_txt hooks' of
    Nothing -> do
      putStrLn $ "no such hpc command : " ++ sub_txt
      exitFailure
    Just plugin' -> do
      command_usage plugin'
      exitSuccess

help_options :: FlagOptSeq
help_options = id

version_plugin :: Plugin
version_plugin =
  Plugin
    { name = "version"
    , usage = ""
    , summary = "Display version for hpc"
    , options = id
    , implementation = version_main
    , init_flags = default_flags
    , final_flags = default_final_flags
    }

version_main :: Flags -> [String] -> IO ()
version_main _ _ = putStrLn ("hpc tools, version " ++ showVersion version)
