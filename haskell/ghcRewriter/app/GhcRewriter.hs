module GhcRewriter (plugin) where

import Lib 
import GhcPlugins

plugin :: Plugin
plugin = defaultPlugin {
  installCoreToDos = install
  }