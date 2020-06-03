{-# LANGUAGE OverloadedStrings #-}

module Main where


import           Termonad
import           Termonad.App
import           Termonad.Config
import           Termonad.Config.Colour
import           Termonad.Config.Vec


myTMConfig :: TMConfig
myTMConfig = defaultTMConfig
  { options = defaultConfigOptions { showScrollbar   = ShowScrollbarNever
                                   , confirmExit     = False
                                   , showMenu        = False
                                   , cursorBlinkMode = CursorBlinkModeOn
                                   , fontConfig      = fontConf
                                   }
  }


fontConf :: FontConfig
fontConf = defaultFontConfig { fontFamily = "Iosevka Nerd Font"
                             , fontSize   = FontSizePoints 14
                             }


gruvboxDark :: ColourConfig (AlphaColour Double)
gruvboxDark = defaultColourConfig
    -- Set the default foreground colour of text of the terminal.
  { foregroundColour = Set (createColour 213 196 161) -- fg2
  , backgroundColour = Set (createColour 40 40 40) -- bg0
    -- Set the extended palette that has 2 Vecs of 8 Gruvbox palette colours
  , palette          = ExtendedPalette gruvboxDark1 gruvboxDark2
  }
 where
  gruvboxDark1 :: Vec N8 (AlphaColour Double)
  gruvboxDark1 =
    createColour 40 40 40 -- bg0
      :* createColour 204 36  29 -- red.1
      :* createColour 152 151 26 -- green.2
      :* createColour 215 153 33 -- yellow.3
      :* createColour 69  133 136 -- blue.4
      :* createColour 177 98  134 -- purple.5
      :* createColour 104 157 106 -- aqua.6
      :* createColour 189 174 147 -- fg3
      :* EmptyVec

  gruvboxDark2 :: Vec N8 (AlphaColour Double)
  gruvboxDark2 =
    createColour 124 111 100 -- bg4
      :* createColour 251 71  52 -- red.9
      :* createColour 184 187 38 -- green.10
      :* createColour 250 189 47 -- yellow.11
      :* createColour 131 165 152 -- blue.12
      :* createColour 211 134 155 -- purple.13
      :* createColour 142 192 124 -- aqua.14
      :* createColour 235 219 178 -- fg1
      :* EmptyVec


main :: IO ()
main = do
  myColourExt <- createColourExtension gruvboxDark
  let newTMConfig = addColourExtension myTMConfig myColourExt
  start newTMConfig
