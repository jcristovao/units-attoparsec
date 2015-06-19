{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE TypeFamilies, DataKinds, DefaultSignatures, MultiParamTypeClasses,
             ConstraintKinds, UndecidableInstances, FlexibleContexts,
             FlexibleInstances, ScopedTypeVariables, TypeOperators, PolyKinds #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes     #-}

module Data.Units.SI.Units.Attoparsec.Text
 ( ParseUnit(..)
 , parseTime

 , gramP
 , meterP
 , metreP
 , secondP
 , minuteP
 , hourP

 , ampereP
 , kelvinP
 , moleP
 , candelaP
 , hertzP
 , literP
 , litreP
 , newtonP
 , pascalP
 , jouleP
 , wattP
 , coloumbP
 , voltP
 , faradP
 , ohmP
 , siemensP
 , weberP
 , teslaP
 , henryP
 , lumenP
 , luxP
 , becquerelP
 , grayP
 , sievertP
 , katalP
 , hectareP
 , tonP
 , tonneP
 )where

import Data.Text (Text)
import Data.Attoparsec.Text hiding (Number)
import Data.Metrology
import qualified Data.Metrology.SI.MonoTypes as Mono
import Data.Units.SI
import Data.Units.SI.Prefixes.Attoparsec.Text

import Data.Metrology.TH

import Control.Applicative
import Control.Monad

class ParseUnit g i where
  parseUnit :: Parser g -> Parser i


build :: (Subset
            (CanonicalUnitsOfFactors (UnitFactorsOf unit))
            (CanonicalUnitsOfFactors
               (LookupList (DimFactorsOf (DimOfUnit unit)) 'DefaultLCSU)),
          Subset
            (CanonicalUnitsOfFactors
               (LookupList (DimFactorsOf (DimOfUnit unit)) 'DefaultLCSU))
            (CanonicalUnitsOfFactors (UnitFactorsOf unit)),
          Unit unit,
          UnitFactor
            (LookupList (DimFactorsOf (DimOfUnit unit)) 'DefaultLCSU))
      => Double
      -> Parser a
      -> (a -> unit)
      -> Parser (Qu (DimFactorsOf (DimOfUnit unit)) 'DefaultLCSU Double)
build v g p = (\u' -> v % p u') <$> (skipSpace >> g)

parseUnit' g = skipSpace >> double >>=
    (\v -> let
      in msum . fmap (\op -> skipSpace >> op) $
      [ decaP  >>= build v g . (:@)
      , hectoP >>= build v g . (:@)
      , kiloP  >>= build v g . (:@)
      , megaP  >>= build v g . (:@)
      , gigaP  >>= build v g . (:@)
      , teraP  >>= build v g . (:@)
      , petaP  >>= build v g . (:@)
      , exaP   >>= build v g . (:@)
      , zettaP >>= build v g . (:@)
      , yottaP >>= build v g . (:@)
      , deciP  >>= build v g . (:@)
      , centiP >>= build v g . (:@)
      , milliP >>= build v g . (:@)
      , microP >>= build v g . (:@)
      , nanoP  >>= build v g . (:@)
      , picoP  >>= build v g . (:@)
      , femtoP >>= build v g . (:@)
      , attoP  >>= build v g . (:@)
      , zeptoP >>= build v g . (:@)
      , yoctoP >>= build v g . (:@)
      -- No prefix
      , (\u' -> v % u') <$> (skipSpace >> g)
      ]
      )

-- | Since the parser code is highly repetitive, let save some characters
(>~) :: Char -> b -> Parser b
a >~  b = char a   >> return b
(>>~):: Text -> b  -> Parser b
a >>~ b = asciiCI a >> return b

meterP :: Parser Meter
meterP = 'm' >~ Meter

metreP :: Parser Meter
metreP = meterP


instance ParseUnit Meter $(evalType [t| Mono.Length |])  where
  parseUnit = parseUnit'

gramP :: Parser Gram
gramP = 'g' >~ Gram

instance ParseUnit Gram $(evalType [t| Mono.Mass |]) where
  parseUnit = parseUnit'

secondP :: Parser Second
secondP =   "seconds"   >>~ Second
        <|> "second"    >>~ Second
        <|> "segundos"  >>~ Second
        <|> "segundo"   >>~ Second
        <|> "secs"      >>~ Second
        <|> "sec"       >>~ Second
        <|> 's' >~ Second

instance ParseUnit Second $(evalType [t| Mono.Time |])  where
  parseUnit = parseUnit'

minuteP :: Parser Minute
minuteP =   "minutes" >>~ Minute
        <|> "minutos" >>~ Minute
        <|> "minute"  >>~ Minute
        <|> "minuto"  >>~ Minute
        <|> "min" >>~ Minute

instance ParseUnit Minute $(evalType [t| Mono.Time |])  where
  parseUnit = parseUnit'

hourP :: Parser Hour
hourP =  "hours" >>~ Hour
     <|> "hour"  >>~ Hour
     <|> 'h' >~ Hour

instance ParseUnit Hour $(evalType [t| Mono.Time |])  where
  parseUnit = parseUnit'

parseTime :: Parser Mono.Time
parseTime =  parseUnit secondP
         <|> parseUnit minuteP
         <|> parseUnit hourP

ampereP :: Parser Ampere
ampereP = 'A' >~ Ampere

kelvinP :: Parser Kelvin
kelvinP = 'k' >~ Kelvin

moleP :: Parser Mole
moleP = "mol" >>~ Mole

candelaP :: Parser Candela
candelaP = "cd" >>~ Candela

hertzP :: Parser Hertz
hertzP = "Hz" >>~ Hertz

literP :: Parser Liter
literP = 'l' >~ Liter

litreP :: Parser Liter
litreP = literP

newtonP :: Parser Newton
newtonP = 'N' >~ Newton

pascalP :: Parser Pascal
pascalP = "Pa" >>~ Pascal

jouleP :: Parser Joule
jouleP = 'J' >~ Joule

wattP :: Parser Watt
wattP = 'W' >~ Watt

coloumbP :: Parser Coulomb
coloumbP = 'C' >~ Coulomb

voltP :: Parser Volt
voltP = 'V' >~ Volt

faradP :: Parser Farad
faradP = 'F' >~ Farad

ohmP :: Parser Ohm
ohmP = 'Ω' >~ Ohm

siemensP :: Parser Siemens
siemensP = 'S' >~ Siemens

weberP :: Parser Weber
weberP = "Wb" >>~ Weber

teslaP :: Parser Tesla
teslaP = 'T' >~ Tesla

henryP :: Parser Henry
henryP = 'H' >~ Henry

lumenP :: Parser Lumen
lumenP = "lm" >>~ Lumen

luxP :: Parser Lux
luxP = "lx" >>~ Lux

becquerelP :: Parser Becquerel
becquerelP = "Bq" >>~ Becquerel

grayP :: Parser Gray
grayP = "Gy" >>~ Gray

sievertP :: Parser Sievert
sievertP = "Sv" >>~ Sievert

katalP :: Parser Katal
katalP = "kat" >>~ Katal

hectareP :: Parser Hectare
hectareP = "ha" >>~ Hectare

tonP :: Parser Ton
tonP = 't' >~ Ton

tonneP :: Parser Ton
tonneP = tonP
