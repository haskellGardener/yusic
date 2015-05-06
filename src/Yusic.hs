{-# LANGUAGE OverloadedStrings #-}

module Yusic
where

import qualified Data.Text as T		-- avoid compiler complaining about collissions with prelude functionality
import Data.List
import Data.Maybe
import Codec.Midi

yusic :: T.Text				-- Yusic only return text - for basic functionality testing
yusic = "Well Hello Dolly"		-- Yusic return text "well hello dolly" - for basic functionality testing

data SigPitch = C  | Cs_Db  | D  | Ds_Eb  | E  | F  | Fs_Gb  | G  | Gs_Ab  | A  | As_Bb  | B  deriving (Show, Ord, Eq)	

-- SigPitch is a datatype\type constructor  which holds the above value constructors representing each musical root.
-- Deriving (Show,Ord,EQ) - derived instance is an instance declaration that is generated automatically in conjunction
--                          with a data or newtype declaration. Derived constructors always traverse constructors from
--                          Left to Right, or here from C - B. 
-- Show - used to coerce values into strings and parse strings into values.
-- Ord  - (/=) - compare arguments lexicographically with respect to the constructor set given, with earlier value
--               constructors in the datatype declaration counting as smaller than later ones (e.g in SigPitch, C is smaller than B.)
-- Eq   - (==) - compare the equality of constructors.
-- Enum - derived instances of Enum only work with data types with only nullary constructors (or a constructor that takes no arguments).
--
--  More on enum defitions: -      enumFrom x           = enumFromTo x lastCon
--  enumFromThen x y     = enumFromThenTo x y bound
--                       where
--                         bound | fromEnum y >= fromEnum x = lastCon
--                               | otherwise                = firstCon
--  enumFromTo x y       = map toEnum [fromEnum x .. fromEnum y]
--  enumFromThenTo x y z = map toEnum [fromEnum x, fromEnum y .. fromEnum z]
--  More on Enum - Below Enum is utilized in Note. In this example [A7..] == [As_Bb7,B7,C8]

data YNote     =                                                              A0 | As_Bb0 | B0
              | C1 | Cs_Db1 | D1 | Ds_Eb1 | E1 | F1 | Fs_Gb1 | G1 | Gs_Ab1 | A1 | As_Bb1 | B1
              | C2 | Cs_Db2 | D2 | Ds_Eb2 | E2 | F2 | Fs_Gb2 | G2 | Gs_Ab2 | A2 | As_Bb2 | B2
              | C3 | Cs_Db3 | D3 | Ds_Eb3 | E3 | F3 | Fs_Gb3 | G3 | Gs_Ab3 | A3 | As_Bb3 | B3
              | C4 | Cs_Db4 | D4 | Ds_Eb4 | E4 | F4 | Fs_Gb4 | G4 | Gs_Ab4 | A4 | As_Bb4 | B4
              | C5 | Cs_Db5 | D5 | Ds_Eb5 | E5 | F5 | Fs_Gb5 | G5 | Gs_Ab5 | A5 | As_Bb5 | B5
              | C6 | Cs_Db6 | D6 | Ds_Eb6 | E6 | F6 | Fs_Gb6 | G6 | Gs_Ab6 | A6 | As_Bb6 | B6
              | C7 | Cs_Db7 | D7 | Ds_Eb7 | E7 | F7 | Fs_Gb7 | G7 | Gs_Ab7 | A7 | As_Bb7 | B7
              | C8                                                                            deriving (Show, Eq, Ord, Enum)


-- Above, YNote is a datatype that holds the above constructors A0-C8 which represent every note in every octave of a piano.
-- Again, Deriving is used to give comparability to the constructors in the list. 

-- Below, KeyGuide is a dataype that holds the first 12 of the possible 51 chords structures possible for each root note.
-- They are denoted as KG_foo as to avoid a name space collision with other constructor names used as we disovered during compilation.

data KeyGuide = KG_CMajor              -- Begin C
              | KG_CMinor
              | KG_C5
              | KG_CDominant7
              | KG_CMajor7
              | KG_CMinor7
              | KG_CMinorMajor7
              | KG_CSus4
              | KG_CSus2
              | KG_C6
              | KG_CMinor6
              | KG_C9                  -- End C

              | KG_Cs_DbMajor          -- Begin Cs/Db
              | KG_Cs_DbMinor
              | KG_Cs_Db5
              | KG_Cs_DbDominant7
              | KG_Cs_DbMajor7
              | KG_Cs_DbMinor7
              | KG_Cs_DbMinorMajor7
              | KG_Cs_DbSus4
              | KG_Cs_DbSus2
              | KG_Cs_Db6
              | KG_Cs_DbMinor6
              | KG_Cs_Db9              -- End Cs/Db

              | KG_DMajor              -- Begin D
              | KG_DMinor
              | KG_D5
              | KG_DDominant7
              | KG_DMajor7
              | KG_DMinor7
              | KG_DMinorMajor7
              | KG_DSus4
              | KG_DSus2
              | KG_D6
              | KG_DMinor6
              | KG_D9                  -- End D

              | KG_Ds_EbMajor          -- Begin Ds/Eb
              | KG_Ds_EbMinor
              | KG_Ds_Eb5
              | KG_Ds_EbDominant7
              | KG_Ds_EbMajor7
              | KG_Ds_EbMinor7
              | KG_Ds_EbMinorMajor7
              | KG_Ds_EbSus4
              | KG_Ds_EbSus2
              | KG_Ds_Eb6
              | KG_Ds_EbMinor6
              | KG_Ds_Eb9              -- End Ds/Eb

              | KG_EMajor              -- Begin E
              | KG_EMinor
              | KG_E5
              | KG_EDominant7
              | KG_EMajor7
              | KG_EMinor7
              | KG_EMinorMajor7
              | KG_ESus4
              | KG_ESus2
              | KG_E6
              | KG_EMinor6
              | KG_E9                  -- End E

              | KG_F_EsMajor           -- Begin F/Es
              | KG_F_EsMinor
              | KG_F_Es5
              | KG_F_EsDominant7
              | KG_F_EsMajor7
              | KG_F_EsMinor7
              | KG_F_EsMinorMajor7
              | KG_F_EsSus4
              | KG_F_EsSus2
              | KG_F_Es6
              | KG_F_EsMinor6
              | KG_F_Es9               -- End F/Es

              | KG_Fs_GbMajor          -- Begin Fs/Gb
              | KG_Fs_GbMinor
              | KG_Fs_Gb5
              | KG_Fs_GbDominant7
              | KG_Fs_GbMajor7
              | KG_Fs_GbMinor7
              | KG_Fs_GbMinorMajor7
              | KG_Fs_GbSus4
              | KG_Fs_GbSus2
              | KG_Fs_Gb6
              | KG_Fs_GbMinor6
              | KG_Fs_Gb9              -- End Fs/Eb

              | KG_GMajor              -- Begin G
              | KG_GMinor
              | KG_G5
              | KG_GDominant7
              | KG_GMajor7
              | KG_GMinor7
              | KG_GMinorMajor7
              | KG_GSus4
              | KG_GSus2
              | KG_G6
              | KG_GMinor6
              | KG_G9                  -- End G

              | KG_Gs_AbMajor          -- Begin Gs/Ab
              | KG_Gs_AbMinor
              | KG_Gs_Ab5
              | KG_Gs_AbDominant7
              | KG_Gs_AbMajor7
              | KG_Gs_AbMinor7
              | KG_Gs_AbMinorMajor7
              | KG_Gs_AbSus4
              | KG_Gs_AbSus2
              | KG_Gs_Ab6
              | KG_Gs_AbMinor6
              | KG_Gs_Ab9              -- End Gs/Ab

              | KG_AMajor              -- Begin A
              | KG_AMinor
              | KG_A5
              | KG_ADominant7
              | KG_AMajor7
              | KG_AMinor7
              | KG_AMinorMajor7
              | KG_ASus4
              | KG_ASus2
              | KG_A6
              | KG_AMinor6
              | KG_A9                  -- End A

              | KG_As_BbMajor          -- Begin As/Bb
              | KG_As_BbMinor
              | KG_As_Bb5
              | KG_As_BbDominant7
              | KG_As_BbMajor7
              | KG_As_BbMinor7
              | KG_As_BbMinorMajor7
              | KG_As_BbSus4
              | KG_As_BbSus2
              | KG_As_Bb6
              | KG_As_BbMinor6
              | KG_As_Bb9              -- End As/Bb

              | KG_B_CbMajor           -- Begin B/Cb
              | KG_B_CbMinor
              | KG_B_Cb5
              | KG_B_CbDominant7
              | KG_B_CbMajor7
              | KG_B_CbMinor7
              | KG_B_CbMinorMajor7
              | KG_B_CbSus4
              | KG_B_CbSus2
              | KG_B_Cb6
              | KG_B_CbMinor6
              | KG_B_Cb9               -- End B/Cb
                deriving (Show, Eq, Ord, Enum)


-- Begin Test Functions

-- allNotes 
allNotes :: [] YNote
allNotes = enumFrom A0

allNotePitches :: [] SigPitch 
allNotePitches = map toSigPitch allNotes

allNoteOctaves :: [] Int
allNoteOctaves = map toOctave allNotes

allPitchOctavePairs :: [] (SigPitch, Int)
allPitchOctavePairs = zip allNotePitches allNoteOctaves

--Endomorphism - An Endomorphism in a given Category is a morphism from some object to itself.  
noteEndomorphismP :: Bool
noteEndomorphismP = allNotes `sEQ` allNotes'
  where
    mF (s,o) = toNoteBySigOctave s o
    allNotes' = catMaybes $ map mF allPitchOctavePairs	   -- catMaybes is part of Data.Maybe: it filters for Just constructors,
                                                           -- then extracts and returns contained values. This is a convienence function.
    	      		    	   			   -- allNotes' should be equal to allNotes
allKeyGuides :: [] KeyGuide
allKeyGuides = enumFrom KG_CMajor

allKeyGuideSigPitches :: [] ([] SigPitch)
allKeyGuideSigPitches = map fromKeyGuide allKeyGuides

keyGuideEndomorphismP :: Bool
keyGuideEndomorphismP = allKeyGuides `sEQ` allKeyGuides'
  where
    allKeyGuides' = catMaybes $ map toKeyGuide allKeyGuideSigPitches

keyGuideRevealNothings :: [] (KeyGuide, (Maybe KeyGuide))
keyGuideRevealNothings = filter (isNothing . snd) pairs
  where
    maybeKeyGuides = map toKeyGuide allKeyGuideSigPitches
    pairs = zip allKeyGuides maybeKeyGuides

keyGuideRevealDifferences :: [] KeyGuide
keyGuideRevealDifferences = (sort . catMaybes $ map toKeyGuide allKeyGuideSigPitches) \\ (sort allKeyGuides)

keyGuideBadPairs :: [] (KeyGuide, KeyGuide)
keyGuideBadPairs = filter (\(kg, kg') -> kg /= kg') $ zip allKeyGuides (catMaybes $ map toKeyGuide allKeyGuideSigPitches)

allToMidis :: [] Int
allToMidis = map toMidi allNotes

noteMidiEndomorphismP :: Bool
noteMidiEndomorphismP = allNotes `sEQ` allNotes'
  where
    allNotes' = catMaybes $ map fromMidi allToMidis

-- End Test Functions





-- Below is the second draft of our toKeyGuide, built originally  with sp to sort and compare both deconstructors and positional parameters.
-- This quickly became an issue for our tests because, although we wanted order of notes not to matter, for flexibility we discovered through tests that, of course
-- some differently named chords may sometimes share the same notes, but in different orders. 
-- 
-- toKeyGuide :: [SigPitch] -> Maybe KeyGuide
-- toKeyGuide [] = Nothing
-- toKeyGuide sp | sp `sEQ` [C,E,G                     ] = Just KG_CMajor     -- sort/compare both deconstructor and positional parameter
--               | sp `sEQ` [C,Ds_Eb,G                 ] = Just KG_CMinor
--               | sp `sEQ` [C,G                       ] = Just KG_C5
--               | sp `sEQ` [C,E,G,As_Bb               ] = Just KG_CDominant7
--               | sp `sEQ` [C,E,G,B                   ] = Just KG_CMajor7
--               | sp `sEQ` [C,Ds_Eb,G,As_Bb           ] = Just KG_CMinor7
--               | sp `sEQ` [C,Ds_Eb,G,B               ] = Just KG_CMinorMajor7
--               | sp `sEQ` [C,F,G                     ] = Just KG_CSus4
--               | sp `sEQ` [C,D,G                     ] = Just KG_CSus2
--               | sp `sEQ` [C,E,G,A                   ] = Just KG_C6
--               | sp `sEQ` [C,Ds_Eb,G,A               ] = Just KG_CMinor6
--               | sp `sEQ` [C,E,G,As_Bb               ] = Just KG_C9
                                                        
--               | sp `sEQ` [Cs_Db,F,Gs_Ab             ] = Just KG_Cs_DbMajor -- Begin Cs_DbMajor
--               | sp `sEQ` [Cs_Db,E,Gs_Ab             ] = Just KG_Cs_DbMinor
--               | sp `sEQ` [Cs_Db,Gs_Ab               ] = Just KG_Cs_Db5
--               | sp `sEQ` [Cs_Db,F,Gs_Ab,B           ] = Just KG_Cs_DbDominant7
--               | sp `sEQ` [Cs_Db,F,Gs_Ab,C           ] = Just KG_Cs_DbMajor7
--               | sp `sEQ` [Cs_Db,E,Gs_Ab,B           ] = Just KG_Cs_DbMinor7
--               | sp `sEQ` [Cs_Db,E,Gs_Ab,C           ] = Just KG_Cs_DbMinorMajor7
--               | sp `sEQ` [Cs_Db,Fs_Gb,Gs_Ab         ] = Just KG_Cs_DbSus4
--               | sp `sEQ` [Cs_Db,Ds_Eb,Gs_Ab         ] = Just KG_Cs_DbSus2
--               | sp `sEQ` [Cs_Db,F,Gs_Ab,As_Bb       ] = Just KG_Cs_Db6
--               | sp `sEQ` [Cs_Db,E,Gs_Ab,As_Bb       ] = Just KG_Cs_DbMinor6
--               | sp `sEQ` [Cs_Db,Ds_Eb,F,Gs_Ab,B     ] = Just KG_Cs_Db9     -- End Cs_Db
                                                        
--               | sp `sEQ` [D,Fs_Gb,A                 ] = Just KG_DMajor     -- Begin D
--               | sp `sEQ` [D,F,A                     ] = Just KG_DMinor
--               | sp `sEQ` [D,A                       ] = Just KG_D5
--               | sp `sEQ` [D,Fs_Gb,A,C               ] = Just KG_DDominant7
--               | sp `sEQ` [D,Fs_Gb,A,Cs_Db           ] = Just KG_DMajor7
--               | sp `sEQ` [D,F,A,C                   ] = Just KG_DMinor7
--               | sp `sEQ` [D,F,G,Cs_Db               ] = Just KG_DMinorMajor7
--               | sp `sEQ` [D,G,A                     ] = Just KG_DSus4
--               | sp `sEQ` [D,E,A                     ] = Just KG_DSus2
--               | sp `sEQ` [D,Fs_Gb,A,B               ] = Just KG_D6
--               | sp `sEQ` [D,F,A,B                   ] = Just KG_DMinor6
--               | sp `sEQ` [D,E,Fs_Gb,A,C             ] = Just KG_D9         -- End DMajor
                                                        
--               | sp `sEQ` [Ds_Eb,G,As_Bb             ] = Just KG_Ds_EbMajor -- Begin Ds_Eb
--               | sp `sEQ` [Ds_Eb,Fs_Gb,As_Bb         ] = Just KG_Ds_EbMinor
--               | sp `sEQ` [Ds_Eb,As_Bb               ] = Just KG_Ds_Eb5
--               | sp `sEQ` [Ds_Eb,G,As_Bb,Cs_Db       ] = Just KG_Ds_EbDominant7
--               | sp `sEQ` [Ds_Eb,G,As_Bb,D           ] = Just KG_Ds_EbMajor7
--               | sp `sEQ` [Ds_Eb,Fs_Gb,As_Bb,Cs_Db   ] = Just KG_Ds_EbMinor7
--               | sp `sEQ` [Ds_Eb,Fs_Gb,As_Bb,D       ] = Just KG_Ds_EbMinorMajor7
--               | sp `sEQ` [Ds_Eb,Gs_Ab,As_Bb         ] = Just KG_Ds_EbSus4
--               | sp `sEQ` [Ds_Eb,F,As_Bb             ] = Just KG_Ds_EbSus2
--               | sp `sEQ` [Ds_Eb,G,As_Bb,C           ] = Just KG_Ds_Eb6
--               | sp `sEQ` [Ds_Eb,Fs_Gb,As_Bb,C       ] = Just KG_Ds_EbMinor6
--               | sp `sEQ` [Ds_Eb,F,G,As_Bb,Cs_Db     ] = Just KG_Ds_Eb9     -- End Ds_Eb
                                                        
--               | sp `sEQ` [E,Gs_Ab,B                 ] = Just KG_EMajor     -- Begin E
--               | sp `sEQ` [E,G,B                     ] = Just KG_EMinor
--               | sp `sEQ` [E,B                       ] = Just KG_E5
--               | sp `sEQ` [E,Gs_Ab,B,D               ] = Just KG_EDominant7
--               | sp `sEQ` [E,Gs_Ab,B,Ds_Eb           ] = Just KG_EMajor7
--               | sp `sEQ` [E,G,B,D                   ] = Just KG_EMinor7
--               | sp `sEQ` [E,G,B,Ds_Eb               ] = Just KG_EMinorMajor7
--               | sp `sEQ` [E,A,B                     ] = Just KG_ESus4
--               | sp `sEQ` [E,Fs_Gb,B                 ] = Just KG_ESus2
--               | sp `sEQ` [E,Gs_Ab,B,Cs_Db           ] = Just KG_E6
--               | sp `sEQ` [E,G,B,Cs_Db               ] = Just KG_EMinor6
--               | sp `sEQ` [E,Fs_Gb,Gs_Ab,B,D         ] = Just KG_E9         -- End E
                                                        
--               | sp `sEQ` [F,A,C                     ] = Just KG_F_EsMajor  -- Begin F_Es
--               | sp `sEQ` [F,Gs_Ab,C                 ] = Just KG_F_EsMinor
--               | sp `sEQ` [F,C                       ] = Just KG_F_Es5
--               | sp `sEQ` [F,G,C,Ds_Eb               ] = Just KG_F_EsDominant7
--               | sp `sEQ` [F,A,C,E                   ] = Just KG_F_EsMajor7
--               | sp `sEQ` [F,Gs_Ab,C,Ds_Eb           ] = Just KG_F_EsMinor7
--               | sp `sEQ` [F,Gs_Ab,C,E               ] = Just KG_F_EsMinorMajor7
--               | sp `sEQ` [F,As_Bb,C                 ] = Just KG_F_EsSus4
--               | sp `sEQ` [F,G,C                     ] = Just KG_F_EsSus2
--               | sp `sEQ` [F,A,C,D                   ] = Just KG_F_Es6
--               | sp `sEQ` [F,Gs_Ab,C,D               ] = Just KG_F_EsMinor6
--               | sp `sEQ` [F,G,A,C,Ds_Eb             ] = Just KG_F_Es9      -- End F_Es
                                                        
--               | sp `sEQ` [Fs_Gb,As_Bb,Cs_Db         ] = Just KG_Fs_GbMajor -- Begin Fs_Gb
--               | sp `sEQ` [Fs_Gb,A,Cs_Db             ] = Just KG_Fs_GbMinor
--               | sp `sEQ` [Fs_Gb,Cs_Db               ] = Just KG_Fs_Gb5
--               | sp `sEQ` [Fs_Gb,As_Bb,Cs_Db,E       ] = Just KG_Fs_GbDominant7
--               | sp `sEQ` [Fs_Gb,As_Bb,F             ] = Just KG_Fs_GbMajor7
--               | sp `sEQ` [Fs_Gb,A,Cs_Db,E           ] = Just KG_Fs_GbMinor7
--               | sp `sEQ` [Fs_Gb,A,Cs_Db,F           ] = Just KG_Fs_GbMinorMajor7
--               | sp `sEQ` [Fs_Gb,B,Cs_Db             ] = Just KG_Fs_GbSus4
--               | sp `sEQ` [Fs_Gb,Gs_Ab,Cs_Db         ] = Just KG_Fs_GbSus2
--               | sp `sEQ` [Fs_Gb,As_Bb,Cs_Db,Ds_Eb   ] = Just KG_Fs_Gb6
--               | sp `sEQ` [Fs_Gb,A,Cs_Db,Ds_Eb       ] = Just KG_Fs_GbMinor6
--               | sp `sEQ` [Fs_Gb,Gs_Ab,As_Bb,Cs_Db,E ] = Just KG_Fs_Gb9     --end Fs_Gb
                                                        
--               | sp `sEQ` [G,B,D                     ] = Just KG_GMajor     --Begin G
--               | sp `sEQ` [G,As_Bb,D                 ] = Just KG_GMinor
--               | sp `sEQ` [G,D                       ] = Just KG_G5
--               | sp `sEQ` [G,B,D,F                   ] = Just KG_GDominant7
--               | sp `sEQ` [G,B,D,Fs_Gb               ] = Just KG_GMajor7
--               | sp `sEQ` [G,As_Bb,D,F               ] = Just KG_GMinor7
--               | sp `sEQ` [G,As_Bb,D,Fs_Gb           ] = Just KG_GMinorMajor7
--               | sp `sEQ` [G,C,D                     ] = Just KG_GSus4
--               | sp `sEQ` [G,A,D                     ] = Just KG_GSus2
--               | sp `sEQ` [G,B,D,E                   ] = Just KG_G6
--               | sp `sEQ` [G,As_Bb,D,E               ] = Just KG_Gs_AbMinor6
--               | sp `sEQ` [G,A,B,D,F                 ] = Just KG_Gs_Ab9     -- End G
                                                        
--               | sp `sEQ` [Gs_Ab,C,Ds_Eb             ] = Just KG_Gs_AbMajor -- Begin Gs_Ab
--               | sp `sEQ` [Gs_Ab,B,Ds_Eb             ] = Just KG_Gs_AbMinor
--               | sp `sEQ` [Gs_Ab,Ds_Eb               ] = Just KG_Gs_Ab5
--               | sp `sEQ` [Gs_Ab,C,Ds_Eb,Cs_Db       ] = Just KG_Gs_AbDominant7
--               | sp `sEQ` [Gs_Ab,C,Ds_Eb,G           ] = Just KG_Gs_AbMajor7
--               | sp `sEQ` [Gs_Ab,B,Ds_Eb,Cs_Db       ] = Just KG_Gs_AbMinor7
--               | sp `sEQ` [Gs_Ab,B,Ds_Eb,G           ] = Just KG_Gs_AbMinorMajor7
--               | sp `sEQ` [Gs_Ab,Cs_Db,Ds_Eb         ] = Just KG_Gs_AbSus4
--               | sp `sEQ` [Gs_Ab,As_Bb,Ds_Eb         ] = Just KG_Gs_AbSus2
--               | sp `sEQ` [Gs_Ab,C,Ds_Eb,F           ] = Just KG_Gs_Ab6
--               | sp `sEQ` [Gs_Ab,B,Ds_Eb,F           ] = Just KG_Gs_AbMinor6
--               | sp `sEQ` [Gs_Ab,As_Bb,C,Ds_Eb,Fs_Gb ] = Just KG_Gs_Ab9     --end Gs_Ab
                                                        
--               | sp `sEQ` [A,Cs_Db,E                 ] = Just KG_AMajor     --Begin A
--               | sp `sEQ` [A,C,E                     ] = Just KG_AMinor
--               | sp `sEQ` [A,E                       ] = Just KG_A5
--               | sp `sEQ` [A,Cs_Db,E,G               ] = Just KG_ADominant7
--               | sp `sEQ` [A,Cs_Db,E,Gs_Ab           ] = Just KG_AMajor7
--               | sp `sEQ` [A,C,E,G                   ] = Just KG_AMinor7
--               | sp `sEQ` [A,C,E,Gs_Ab               ] = Just KG_AMinorMajor7
--               | sp `sEQ` [A,D,E                     ] = Just KG_ASus4
--               | sp `sEQ` [A,B,E                     ] = Just KG_ASus2
--               | sp `sEQ` [A,Cs_Db,Fs_Gb             ] = Just KG_A6
--               | sp `sEQ` [A,C,E,Fs_Gb               ] = Just KG_AMinor6
--               | sp `sEQ` [A,B,Cs_Db,E,G             ] = Just KG_A9         -- End A
                                                        
--               | sp `sEQ` [As_Bb,D,F                 ] = Just KG_As_BbMajor -- Begin As_Bb
--               | sp `sEQ` [As_Bb,Cs_Db,F             ] = Just KG_As_BbMinor
--               | sp `sEQ` [As_Bb,F                   ] = Just KG_As_Bb5
--               | sp `sEQ` [As_Bb,D,F,Gs_Ab           ] = Just KG_As_BbDominant7
--               | sp `sEQ` [As_Bb,D,F,A               ] = Just KG_As_BbMajor7
--               | sp `sEQ` [As_Bb,Cs_Db,F,Gs_Ab       ] = Just KG_As_BbMinor7
--               | sp `sEQ` [As_Bb,Cs_Db,F,A           ] = Just KG_As_BbMinorMajor7
--               | sp `sEQ` [As_Bb,Ds_Eb,F             ] = Just KG_As_BbSus4
--               | sp `sEQ` [As_Bb,C,F                 ] = Just KG_As_BbSus2
--               | sp `sEQ` [As_Bb,D,F,G               ] = Just KG_As_Bb6
--               | sp `sEQ` [As_Bb,Cs_Db,F,G           ] = Just KG_As_BbMinor6
--               | sp `sEQ` [As_Bb,C,D,F,Gs_Ab         ] = Just KG_As_Bb9     -- End As_Bb
                                                        
--               | sp `sEQ` [B,Ds_Eb,Fs_Gb             ] = Just KG_B_CbMajor  -- Begin B_Cb
--               | sp `sEQ` [B,D,Fs_Gb                 ] = Just KG_B_CbMinor
--               | sp `sEQ` [B,Fs_Gb                   ] = Just KG_B_Cb5
--               | sp `sEQ` [B,Ds_Eb,Fs_Gb,A           ] = Just KG_B_CbDominant7
--               | sp `sEQ` [B,Ds_Eb,Fs_Gb,As_Bb       ] = Just KG_B_CbMajor7
--               | sp `sEQ` [B,D,Fs_Gb,A               ] = Just KG_B_CbMinor7
--               | sp `sEQ` [B,D,Fs_Gb,As_Bb           ] = Just KG_B_CbMinorMajor7
--               | sp `sEQ` [B,E,Fs_Gb                 ] = Just KG_B_CbSus4
--               | sp `sEQ` [B,Cs_Db,Fs_Gb             ] = Just KG_B_CbSus2
--               | sp `sEQ` [B,Ds_Eb,Fs_Gb,Gs_Ab       ] = Just KG_B_Cb6
--               | sp `sEQ` [B,D,Fs_Gb,Gs_Ab           ] = Just KG_B_CbMinor6
--               | sp `sEQ` [B,Cs_Db,Ds_Eb,Fs_Gb,A     ] = Just KG_B_Cb9      -- End B_Cb
--               | otherwise                             = Nothing            -- Aberrant [SigPitch] deserves Nothing !!!

sEQ :: (Eq a, Ord a) => [a] -> [a] -> Bool    -- Sort and compare two lists
sEQ a b = sort a == sort b

-- toKeyGuide is a function that takes a SigPitch and returns a Maybe KeyGuide.toKeyGuide allows us to determine chords from a list of SigPitch's.
-- Here it's necessary to discuss the Maybe type in Haskell.
-- Maybe type definition  data Maybe a = Just a | Nothing
--                                     deriving (Eq, Ord)
-- Maybe allows a programmer to "specify something is not there". Without this, we would have constant compilation issues for bogus purposes. 
-- Just allows us to return something that is just something. This means we can return something if a value is not present. Again, avoiding compilation issues.
-- Notice the toKeyGuide_ = nothing at the bottom of the toKeyGuide.This simply means we get nothing if we don't find something. 

toKeyGuide :: [SigPitch] -> Maybe KeyGuide
toKeyGuide [] = Nothing
toKeyGuide (C:E:G                     :[]) = Just KG_CMajor     -- sort/compare both deconstructor and positional parameter
toKeyGuide (C:Ds_Eb:G                 :[]) = Just KG_CMinor
toKeyGuide (C:G                       :[]) = Just KG_C5
toKeyGuide (C:E:G:As_Bb               :[]) = Just KG_CDominant7
toKeyGuide (C:E:G:B                   :[]) = Just KG_CMajor7
toKeyGuide (C:Ds_Eb:G:As_Bb           :[]) = Just KG_CMinor7
toKeyGuide (C:Ds_Eb:G:B               :[]) = Just KG_CMinorMajor7
toKeyGuide (C:F:G                     :[]) = Just KG_CSus4
toKeyGuide (C:D:G                     :[]) = Just KG_CSus2
toKeyGuide (C:E:G:A                   :[]) = Just KG_C6
toKeyGuide (C:Ds_Eb:G:A               :[]) = Just KG_CMinor6
toKeyGuide (C:D:E:G:As_Bb             :[]) = Just KG_C9
                                                        
toKeyGuide (Cs_Db:F:Gs_Ab             :[]) = Just KG_Cs_DbMajor -- Begin Cs_DbMajor
toKeyGuide (Cs_Db:E:Gs_Ab             :[]) = Just KG_Cs_DbMinor
toKeyGuide (Cs_Db:Gs_Ab               :[]) = Just KG_Cs_Db5
toKeyGuide (Cs_Db:F:Gs_Ab:B           :[]) = Just KG_Cs_DbDominant7
toKeyGuide (Cs_Db:F:Gs_Ab:C           :[]) = Just KG_Cs_DbMajor7
toKeyGuide (Cs_Db:E:Gs_Ab:B           :[]) = Just KG_Cs_DbMinor7
toKeyGuide (Cs_Db:E:Gs_Ab:C           :[]) = Just KG_Cs_DbMinorMajor7
toKeyGuide (Cs_Db:Fs_Gb:Gs_Ab         :[]) = Just KG_Cs_DbSus4
toKeyGuide (Cs_Db:Ds_Eb:Gs_Ab         :[]) = Just KG_Cs_DbSus2
toKeyGuide (Cs_Db:F:Gs_Ab:As_Bb       :[]) = Just KG_Cs_Db6
toKeyGuide (Cs_Db:E:Gs_Ab:As_Bb       :[]) = Just KG_Cs_DbMinor6
toKeyGuide (Cs_Db:Ds_Eb:F:Gs_Ab:B     :[]) = Just KG_Cs_Db9     -- End Cs_Db
                                                        
toKeyGuide (D:Fs_Gb:A                 :[]) = Just KG_DMajor     -- Begin D
toKeyGuide (D:F:A                     :[]) = Just KG_DMinor
toKeyGuide (D:A                       :[]) = Just KG_D5
toKeyGuide (D:Fs_Gb:A:C               :[]) = Just KG_DDominant7
toKeyGuide (D:Fs_Gb:A:Cs_Db           :[]) = Just KG_DMajor7
toKeyGuide (D:F:A:C                   :[]) = Just KG_DMinor7
toKeyGuide (D:F:G:Cs_Db               :[]) = Just KG_DMinorMajor7
toKeyGuide (D:G:A                     :[]) = Just KG_DSus4
toKeyGuide (D:E:A                     :[]) = Just KG_DSus2
toKeyGuide (D:Fs_Gb:A:B               :[]) = Just KG_D6
toKeyGuide (D:F:A:B                   :[]) = Just KG_DMinor6
toKeyGuide (D:E:Fs_Gb:A:C             :[]) = Just KG_D9         -- End DMajor
                                                        
toKeyGuide (Ds_Eb:G:As_Bb             :[]) = Just KG_Ds_EbMajor -- Begin Ds_Eb
toKeyGuide (Ds_Eb:Fs_Gb:As_Bb         :[]) = Just KG_Ds_EbMinor
toKeyGuide (Ds_Eb:As_Bb               :[]) = Just KG_Ds_Eb5
toKeyGuide (Ds_Eb:G:As_Bb:Cs_Db       :[]) = Just KG_Ds_EbDominant7
toKeyGuide (Ds_Eb:G:As_Bb:D           :[]) = Just KG_Ds_EbMajor7
toKeyGuide (Ds_Eb:Fs_Gb:As_Bb:Cs_Db   :[]) = Just KG_Ds_EbMinor7
toKeyGuide (Ds_Eb:Fs_Gb:As_Bb:D       :[]) = Just KG_Ds_EbMinorMajor7
toKeyGuide (Ds_Eb:Gs_Ab:As_Bb         :[]) = Just KG_Ds_EbSus4
toKeyGuide (Ds_Eb:F:As_Bb             :[]) = Just KG_Ds_EbSus2
toKeyGuide (Ds_Eb:G:As_Bb:C           :[]) = Just KG_Ds_Eb6
toKeyGuide (Ds_Eb:Fs_Gb:As_Bb:C       :[]) = Just KG_Ds_EbMinor6
toKeyGuide (Ds_Eb:F:G:As_Bb:Cs_Db     :[]) = Just KG_Ds_Eb9     -- End Ds_Eb
                                                        
toKeyGuide (E:Gs_Ab:B                 :[]) = Just KG_EMajor     -- Begin E
toKeyGuide (E:G:B                     :[]) = Just KG_EMinor
toKeyGuide (E:B                       :[]) = Just KG_E5
toKeyGuide (E:Gs_Ab:B:D               :[]) = Just KG_EDominant7
toKeyGuide (E:Gs_Ab:B:Ds_Eb           :[]) = Just KG_EMajor7
toKeyGuide (E:G:B:D                   :[]) = Just KG_EMinor7
toKeyGuide (E:G:B:Ds_Eb               :[]) = Just KG_EMinorMajor7
toKeyGuide (E:A:B                     :[]) = Just KG_ESus4
toKeyGuide (E:Fs_Gb:B                 :[]) = Just KG_ESus2
toKeyGuide (E:Gs_Ab:B:Cs_Db           :[]) = Just KG_E6
toKeyGuide (E:G:B:Cs_Db               :[]) = Just KG_EMinor6
toKeyGuide (E:Fs_Gb:Gs_Ab:B:D         :[]) = Just KG_E9         -- End E
                                                        
toKeyGuide (F:A:C                     :[]) = Just KG_F_EsMajor  -- Begin F_Es
toKeyGuide (F:Gs_Ab:C                 :[]) = Just KG_F_EsMinor
toKeyGuide (F:C                       :[]) = Just KG_F_Es5
toKeyGuide (F:G:C:Ds_Eb               :[]) = Just KG_F_EsDominant7
toKeyGuide (F:A:C:E                   :[]) = Just KG_F_EsMajor7
toKeyGuide (F:Gs_Ab:C:Ds_Eb           :[]) = Just KG_F_EsMinor7
toKeyGuide (F:Gs_Ab:C:E               :[]) = Just KG_F_EsMinorMajor7
toKeyGuide (F:As_Bb:C                 :[]) = Just KG_F_EsSus4
toKeyGuide (F:G:C                     :[]) = Just KG_F_EsSus2
toKeyGuide (F:A:C:D                   :[]) = Just KG_F_Es6
toKeyGuide (F:Gs_Ab:C:D               :[]) = Just KG_F_EsMinor6
toKeyGuide (F:G:A:C:Ds_Eb             :[]) = Just KG_F_Es9      -- End F_Es
                                                        
toKeyGuide (Fs_Gb:As_Bb:Cs_Db         :[]) = Just KG_Fs_GbMajor -- Begin Fs_Gb
toKeyGuide (Fs_Gb:A:Cs_Db             :[]) = Just KG_Fs_GbMinor
toKeyGuide (Fs_Gb:Cs_Db               :[]) = Just KG_Fs_Gb5
toKeyGuide (Fs_Gb:As_Bb:Cs_Db:E       :[]) = Just KG_Fs_GbDominant7
toKeyGuide (Fs_Gb:As_Bb:F             :[]) = Just KG_Fs_GbMajor7
toKeyGuide (Fs_Gb:A:Cs_Db:E           :[]) = Just KG_Fs_GbMinor7
toKeyGuide (Fs_Gb:A:Cs_Db:F           :[]) = Just KG_Fs_GbMinorMajor7
toKeyGuide (Fs_Gb:B:Cs_Db             :[]) = Just KG_Fs_GbSus4
toKeyGuide (Fs_Gb:Gs_Ab:Cs_Db         :[]) = Just KG_Fs_GbSus2
toKeyGuide (Fs_Gb:As_Bb:Cs_Db:Ds_Eb   :[]) = Just KG_Fs_Gb6
toKeyGuide (Fs_Gb:A:Cs_Db:Ds_Eb       :[]) = Just KG_Fs_GbMinor6
toKeyGuide (Fs_Gb:Gs_Ab:As_Bb:Cs_Db:E :[]) = Just KG_Fs_Gb9     --end Fs_Gb
                                                        
toKeyGuide (G:B:D                     :[]) = Just KG_GMajor     --Begin G
toKeyGuide (G:As_Bb:D                 :[]) = Just KG_GMinor
toKeyGuide (G:D                       :[]) = Just KG_G5
toKeyGuide (G:B:D:F                   :[]) = Just KG_GDominant7
toKeyGuide (G:B:D:Fs_Gb               :[]) = Just KG_GMajor7
toKeyGuide (G:As_Bb:D:F               :[]) = Just KG_GMinor7
toKeyGuide (G:As_Bb:D:Fs_Gb           :[]) = Just KG_GMinorMajor7
toKeyGuide (G:C:D                     :[]) = Just KG_GSus4
toKeyGuide (G:A:D                     :[]) = Just KG_GSus2
toKeyGuide (G:B:D:E                   :[]) = Just KG_G6
toKeyGuide (G:As_Bb:D:E               :[]) = Just KG_Gs_AbMinor6
toKeyGuide (G:A:B:D:F                 :[]) = Just KG_Gs_Ab9     -- End G
                                                        
toKeyGuide (Gs_Ab:C:Ds_Eb             :[]) = Just KG_Gs_AbMajor -- Begin Gs_Ab
toKeyGuide (Gs_Ab:B:Ds_Eb             :[]) = Just KG_Gs_AbMinor
toKeyGuide (Gs_Ab:Ds_Eb               :[]) = Just KG_Gs_Ab5
toKeyGuide (Gs_Ab:C:Ds_Eb:Cs_Db       :[]) = Just KG_Gs_AbDominant7
toKeyGuide (Gs_Ab:C:Ds_Eb:G           :[]) = Just KG_Gs_AbMajor7
toKeyGuide (Gs_Ab:B:Ds_Eb:Cs_Db       :[]) = Just KG_Gs_AbMinor7
toKeyGuide (Gs_Ab:B:Ds_Eb:G           :[]) = Just KG_Gs_AbMinorMajor7
toKeyGuide (Gs_Ab:Cs_Db:Ds_Eb         :[]) = Just KG_Gs_AbSus4
toKeyGuide (Gs_Ab:As_Bb:Ds_Eb         :[]) = Just KG_Gs_AbSus2
toKeyGuide (Gs_Ab:C:Ds_Eb:F           :[]) = Just KG_Gs_Ab6
toKeyGuide (Gs_Ab:B:Ds_Eb:F           :[]) = Just KG_Gs_AbMinor6
toKeyGuide (Gs_Ab:As_Bb:C:Ds_Eb:Fs_Gb :[]) = Just KG_Gs_Ab9     --end Gs_Ab
                                                        
toKeyGuide (A:Cs_Db:E                 :[]) = Just KG_AMajor     --Begin A
toKeyGuide (A:C:E                     :[]) = Just KG_AMinor
toKeyGuide (A:E                       :[]) = Just KG_A5
toKeyGuide (A:Cs_Db:E:G               :[]) = Just KG_ADominant7
toKeyGuide (A:Cs_Db:E:Gs_Ab           :[]) = Just KG_AMajor7
toKeyGuide (A:C:E:G                   :[]) = Just KG_AMinor7
toKeyGuide (A:C:E:Gs_Ab               :[]) = Just KG_AMinorMajor7
toKeyGuide (A:D:E                     :[]) = Just KG_ASus4
toKeyGuide (A:B:E                     :[]) = Just KG_ASus2
toKeyGuide (A:Cs_Db:Fs_Gb             :[]) = Just KG_A6
toKeyGuide (A:C:E:Fs_Gb               :[]) = Just KG_AMinor6
toKeyGuide (A:B:Cs_Db:E:G             :[]) = Just KG_A9         -- End A
                                                        
toKeyGuide (As_Bb:D:F                 :[]) = Just KG_As_BbMajor -- Begin As_Bb
toKeyGuide (As_Bb:Cs_Db:F             :[]) = Just KG_As_BbMinor
toKeyGuide (As_Bb:F                   :[]) = Just KG_As_Bb5
toKeyGuide (As_Bb:D:F:Gs_Ab           :[]) = Just KG_As_BbDominant7
toKeyGuide (As_Bb:D:F:A               :[]) = Just KG_As_BbMajor7
toKeyGuide (As_Bb:Cs_Db:F:Gs_Ab       :[]) = Just KG_As_BbMinor7
toKeyGuide (As_Bb:Cs_Db:F:A           :[]) = Just KG_As_BbMinorMajor7
toKeyGuide (As_Bb:Ds_Eb:F             :[]) = Just KG_As_BbSus4
toKeyGuide (As_Bb:C:F                 :[]) = Just KG_As_BbSus2
toKeyGuide (As_Bb:D:F:G               :[]) = Just KG_As_Bb6
toKeyGuide (As_Bb:Cs_Db:F:G           :[]) = Just KG_As_BbMinor6
toKeyGuide (As_Bb:C:D:F:Gs_Ab         :[]) = Just KG_As_Bb9     -- End As_Bb
                                                        
toKeyGuide (B:Ds_Eb:Fs_Gb             :[]) = Just KG_B_CbMajor  -- Begin B_Cb
toKeyGuide (B:D:Fs_Gb                 :[]) = Just KG_B_CbMinor
toKeyGuide (B:Fs_Gb                   :[]) = Just KG_B_Cb5
toKeyGuide (B:Ds_Eb:Fs_Gb:A           :[]) = Just KG_B_CbDominant7
toKeyGuide (B:Ds_Eb:Fs_Gb:As_Bb       :[]) = Just KG_B_CbMajor7
toKeyGuide (B:D:Fs_Gb:A               :[]) = Just KG_B_CbMinor7
toKeyGuide (B:D:Fs_Gb:As_Bb           :[]) = Just KG_B_CbMinorMajor7
toKeyGuide (B:E:Fs_Gb                 :[]) = Just KG_B_CbSus4
toKeyGuide (B:Cs_Db:Fs_Gb             :[]) = Just KG_B_CbSus2
toKeyGuide (B:Ds_Eb:Fs_Gb:Gs_Ab       :[]) = Just KG_B_Cb6
toKeyGuide (B:D:Fs_Gb:Gs_Ab           :[]) = Just KG_B_CbMinor6
toKeyGuide (B:Cs_Db:Ds_Eb:Fs_Gb:A     :[]) = Just KG_B_Cb9      -- End B_Cb
toKeyGuide _                               = Nothing            -- Aberrant [SigPitch] deserves Nothing !!!


-- fromKeyGuide is a function that takes a KeyGuide and returns a list of SigPitchs. 

fromKeyGuide :: KeyGuide        -> [] SigPitch
fromKeyGuide KG_CMajor           = C:E:G                     :[] -- Begin C
fromKeyGuide KG_CMinor           = C:Ds_Eb:G                 :[]
fromKeyGuide KG_C5               = C:G                       :[]
fromKeyGuide KG_CDominant7       = C:E:G:As_Bb               :[]
fromKeyGuide KG_CMajor7          = C:E:G:B                   :[]
fromKeyGuide KG_CMinor7          = C:Ds_Eb:G:As_Bb           :[]
fromKeyGuide KG_CMinorMajor7     = C:Ds_Eb:G:B               :[]
fromKeyGuide KG_CSus4            = C:F:G                     :[]
fromKeyGuide KG_CSus2            = C:D:G                     :[]
fromKeyGuide KG_C6               = C:E:G:A                   :[]
fromKeyGuide KG_CMinor6          = C:Ds_Eb:G:A               :[]
fromKeyGuide KG_C9               = C:E:G:As_Bb               :[] -- End C

fromKeyGuide KG_Cs_DbMajor       = Cs_Db:F:Gs_Ab             :[] -- BeginCs/Db
fromKeyGuide KG_Cs_DbMinor       = Cs_Db:E:Gs_Ab             :[]
fromKeyGuide KG_Cs_Db5           = Cs_Db:Gs_Ab               :[]
fromKeyGuide KG_Cs_DbDominant7   = Cs_Db:F:Gs_Ab:B           :[]
fromKeyGuide KG_Cs_DbMajor7      = Cs_Db:F:Gs_Ab:C           :[]
fromKeyGuide KG_Cs_DbMinor7      = Cs_Db:E:Gs_Ab:B           :[]
fromKeyGuide KG_Cs_DbMinorMajor7 = Cs_Db:E:Gs_Ab:C           :[]
fromKeyGuide KG_Cs_DbSus4        = Cs_Db:Fs_Gb:Gs_Ab         :[]
fromKeyGuide KG_Cs_DbSus2        = Cs_Db:Ds_Eb:Gs_Ab         :[]
fromKeyGuide KG_Cs_Db6           = Cs_Db:F:Gs_Ab:As_Bb       :[]
fromKeyGuide KG_Cs_DbMinor6      = Cs_Db:E:Gs_Ab:As_Bb       :[]
fromKeyGuide KG_Cs_Db9           = Cs_Db:Ds_Eb:F:Gs_Ab:B     :[]

fromKeyGuide KG_DMajor           = D:Fs_Gb:A                 :[] --Begin D
fromKeyGuide KG_DMinor           = D:F:A                     :[]
fromKeyGuide KG_D5               = D:A                       :[]
fromKeyGuide KG_DDominant7       = D:Fs_Gb:A:C               :[]
fromKeyGuide KG_DMajor7          = D:Fs_Gb:A:Cs_Db           :[]
fromKeyGuide KG_DMinor7          = D:F:A:C                   :[]
fromKeyGuide KG_DMinorMajor7     = D:F:G:Cs_Db               :[]
fromKeyGuide KG_DSus4            = D:G:A                     :[]
fromKeyGuide KG_DSus2            = D:E:A                     :[]
fromKeyGuide KG_D6               = D:Fs_Gb:A:B               :[]
fromKeyGuide KG_DMinor6          = D:F:A:B                   :[]
fromKeyGuide KG_D9               = D:E:Fs_Gb:A:C             :[] --End D

fromKeyGuide KG_Ds_EbMajor       = Ds_Eb:G:As_Bb             :[] --Begin Ds_Eb
fromKeyGuide KG_Ds_EbMinor       = Ds_Eb:Fs_Gb:As_Bb         :[]
fromKeyGuide KG_Ds_Eb5           = Ds_Eb:As_Bb               :[]
fromKeyGuide KG_Ds_EbDominant7   = Ds_Eb:G:As_Bb:Cs_Db       :[]
fromKeyGuide KG_Ds_EbMajor7      = Ds_Eb:G:As_Bb:D           :[]
fromKeyGuide KG_Ds_EbMinor7      = Ds_Eb:Fs_Gb:As_Bb:Cs_Db   :[]
fromKeyGuide KG_Ds_EbMinorMajor7 = Ds_Eb:Fs_Gb:As_Bb:D       :[]
fromKeyGuide KG_Ds_EbSus4        = Ds_Eb:Gs_Ab:As_Bb         :[]
fromKeyGuide KG_Ds_EbSus2        = Ds_Eb:F:As_Bb             :[]
fromKeyGuide KG_Ds_Eb6           = Ds_Eb:G:As_Bb:C           :[]
fromKeyGuide KG_Ds_EbMinor6      = Ds_Eb:Fs_Gb:As_Bb:C       :[]
fromKeyGuide KG_Ds_Eb9           = Ds_Eb:F:G:As_Bb:Cs_Db     :[] --End Ds_Eb

fromKeyGuide KG_EMajor           = E:Gs_Ab:B                 :[] --Begin E
fromKeyGuide KG_EMinor           = E:G:B                     :[]
fromKeyGuide KG_E5               = E:B                       :[]
fromKeyGuide KG_EDominant7       = E:Gs_Ab:B:D               :[]
fromKeyGuide KG_EMajor7          = E:Gs_Ab:B:Ds_Eb           :[]
fromKeyGuide KG_EMinor7          = E:G:B:D                   :[]
fromKeyGuide KG_EMinorMajor7     = E:G:B:Ds_Eb               :[]
fromKeyGuide KG_ESus4            = E:A:B                     :[]
fromKeyGuide KG_ESus2            = E:Fs_Gb:B                 :[]
fromKeyGuide KG_E6               = E:Gs_Ab:B:Cs_Db           :[]
fromKeyGuide KG_EMinor6          = E:G:B:Cs_Db               :[]
fromKeyGuide KG_E9               = E:Fs_Gb:Gs_Ab:B:D         :[] -- End E

fromKeyGuide KG_F_EsMajor        = F:A:C                     :[] -- Begin F_Es
fromKeyGuide KG_F_EsMinor        = F:Gs_Ab:C                 :[]
fromKeyGuide KG_F_Es5            = F:C                       :[]
fromKeyGuide KG_F_EsDominant7    = F:G:C:Ds_Eb               :[]
fromKeyGuide KG_F_EsMajor7       = F:A:C:E                   :[]
fromKeyGuide KG_F_EsMinor7       = F:Gs_Ab:C:Ds_Eb           :[]
fromKeyGuide KG_F_EsMinorMajor7  = F:Gs_Ab:C:E               :[]
fromKeyGuide KG_F_EsSus4         = F:As_Bb:C                 :[]
fromKeyGuide KG_F_EsSus2         = F:G:C                     :[]
fromKeyGuide KG_F_Es6            = F:A:C:D                   :[]
fromKeyGuide KG_F_EsMinor6       = F:Gs_Ab:C:D               :[]
fromKeyGuide KG_F_Es9            = F:G:A:C:Ds_Eb             :[] -- End F_Es

fromKeyGuide KG_Fs_GbMajor       = Fs_Gb:As_Bb:Cs_Db         :[] -- Begin Fs_Gb
fromKeyGuide KG_Fs_GbMinor       = Fs_Gb:A:Cs_Db             :[]
fromKeyGuide KG_Fs_Gb5           = Fs_Gb:Cs_Db               :[]
fromKeyGuide KG_Fs_GbDominant7   = Fs_Gb:As_Bb:Cs_Db:E       :[]
fromKeyGuide KG_Fs_GbMajor7      = Fs_Gb:As_Bb:F             :[]
fromKeyGuide KG_Fs_GbMinor7      = Fs_Gb:A:Cs_Db:E           :[]
fromKeyGuide KG_Fs_GbMinorMajor7 = Fs_Gb:A:Cs_Db:F           :[]
fromKeyGuide KG_Fs_GbSus4        = Fs_Gb:B:Cs_Db             :[]
fromKeyGuide KG_Fs_GbSus2        = Fs_Gb:Gs_Ab:Cs_Db         :[]
fromKeyGuide KG_Fs_Gb6           = Fs_Gb:As_Bb:Cs_Db:Ds_Eb   :[]
fromKeyGuide KG_Fs_GbMinor6      = Fs_Gb:A:Cs_Db:Ds_Eb       :[]
fromKeyGuide KG_Fs_Gb9           = Fs_Gb:Gs_Ab:As_Bb:Cs_Db:E :[] -- End Fs_Gb

fromKeyGuide KG_GMajor           = G:B:D                     :[] -- Begin G
fromKeyGuide KG_GMinor           = G:As_Bb:D                 :[]
fromKeyGuide KG_G5               = G:D                       :[]
fromKeyGuide KG_GDominant7       = G:B:D:F                   :[]
fromKeyGuide KG_GMajor7          = G:B:D:Fs_Gb               :[]
fromKeyGuide KG_GMinor7          = G:As_Bb:D:F               :[]
fromKeyGuide KG_GMinorMajor7     = G:As_Bb:D:Fs_Gb           :[]
fromKeyGuide KG_GSus4            = G:C:D                     :[]
fromKeyGuide KG_GSus2            = G:A:D                     :[]
fromKeyGuide KG_G6               = G:B:D:E                   :[]
fromKeyGuide KG_GMinor6          = G:As_Bb:D:E               :[]
fromKeyGuide KG_G9               = G:A:B:D:F                 :[] -- End G

fromKeyGuide KG_Gs_AbMajor       = Gs_Ab:C:Ds_Eb             :[] -- Begin Gs_Ab
fromKeyGuide KG_Gs_AbMinor       = Gs_Ab:B:Ds_Eb             :[]
fromKeyGuide KG_Gs_Ab5           = Gs_Ab:Ds_Eb               :[]
fromKeyGuide KG_Gs_AbDominant7   = Gs_Ab:C:Ds_Eb:Cs_Db       :[]
fromKeyGuide KG_Gs_AbMajor7      = Gs_Ab:C:Ds_Eb:G           :[]
fromKeyGuide KG_Gs_AbMinor7      = Gs_Ab:B:Ds_Eb:Cs_Db       :[]
fromKeyGuide KG_Gs_AbMinorMajor7 = Gs_Ab:B:Ds_Eb:G           :[]
fromKeyGuide KG_Gs_AbSus4        = Gs_Ab:Cs_Db:Ds_Eb         :[]
fromKeyGuide KG_Gs_AbSus2        = Gs_Ab:As_Bb:Ds_Eb         :[]
fromKeyGuide KG_Gs_Ab6           = Gs_Ab:C:Ds_Eb:F           :[]
fromKeyGuide KG_Gs_AbMinor6      = Gs_Ab:B:Ds_Eb:F           :[]
fromKeyGuide KG_Gs_Ab9           = Gs_Ab:As_Bb:C:Ds_Eb:Fs_Gb :[] -- End Gs_Ab

fromKeyGuide KG_AMajor           = A:Cs_Db:E                 :[] -- Begin A
fromKeyGuide KG_AMinor           = A:C:E                     :[]
fromKeyGuide KG_A5               = A:E                       :[]
fromKeyGuide KG_ADominant7       = A:Cs_Db:E:G               :[]
fromKeyGuide KG_AMajor7          = A:Cs_Db:E:Gs_Ab           :[]
fromKeyGuide KG_AMinor7          = A:C:E:G                   :[]
fromKeyGuide KG_AMinorMajor7     = A:C:E:Gs_Ab               :[]
fromKeyGuide KG_ASus4            = A:D:E                     :[]
fromKeyGuide KG_ASus2            = A:B:E                     :[]
fromKeyGuide KG_A6               = A:Cs_Db:Fs_Gb             :[]
fromKeyGuide KG_AMinor6          = A:C:E:Fs_Gb               :[]
fromKeyGuide KG_A9               = A:B:Cs_Db:E:G             :[] -- End A

fromKeyGuide KG_As_BbMajor       = As_Bb:D:F                 :[] -- Begin As_Bb
fromKeyGuide KG_As_BbMinor       = As_Bb:Cs_Db:F             :[]
fromKeyGuide KG_As_Bb5           = As_Bb:F                   :[]
fromKeyGuide KG_As_BbDominant7   = As_Bb:D:F:Gs_Ab           :[]
fromKeyGuide KG_As_BbMajor7      = As_Bb:D:F:A               :[]
fromKeyGuide KG_As_BbMinor7      = As_Bb:Cs_Db:F:Gs_Ab       :[]
fromKeyGuide KG_As_BbMinorMajor7 = As_Bb:Cs_Db:F:A           :[]
fromKeyGuide KG_As_BbSus4        = As_Bb:Ds_Eb:F             :[]
fromKeyGuide KG_As_BbSus2        = As_Bb:C:F                 :[]
fromKeyGuide KG_As_Bb6           = As_Bb:D:F:G               :[]
fromKeyGuide KG_As_BbMinor6      = As_Bb:Cs_Db:F:G           :[]
fromKeyGuide KG_As_Bb9           = As_Bb:C:D:F:Gs_Ab         :[] -- End As_Bb

fromKeyGuide KG_B_CbMajor        = B:Ds_Eb:Fs_Gb             :[] -- Begin B_Cb
fromKeyGuide KG_B_CbMinor        = B:D:Fs_Gb                 :[]
fromKeyGuide KG_B_Cb5            = B:Fs_Gb                   :[]
fromKeyGuide KG_B_CbDominant7    = B:Ds_Eb:Fs_Gb:A           :[]
fromKeyGuide KG_B_CbMajor7       = B:Ds_Eb:Fs_Gb:As_Bb       :[]
fromKeyGuide KG_B_CbMinor7       = B:D:Fs_Gb:A               :[]
fromKeyGuide KG_B_CbMinorMajor7  = B:D:Fs_Gb:As_Bb           :[]
fromKeyGuide KG_B_CbSus4         = B:E:Fs_Gb                 :[]
fromKeyGuide KG_B_CbSus2         = B:Cs_Db:Fs_Gb             :[]
fromKeyGuide KG_B_Cb6            = B:Ds_Eb:Fs_Gb:Gs_Ab       :[]
fromKeyGuide KG_B_CbMinor6       = B:D:Fs_Gb:Gs_Ab           :[]
fromKeyGuide KG_B_Cb9            = B:Cs_Db:Ds_Eb:Fs_Gb:A     :[] -- End B_Cb


-- toNotesbySigPich takes a SigPitch and returns a list of YNotes from all piano octave ranges. 

toNotesBySigPitch :: SigPitch -> [] YNote
toNotesBySigPitch A     = A0     :A1     :A2     :A3     :A4     :A5     :A6     :A7         :[]
toNotesBySigPitch As_Bb = As_Bb0 :As_Bb1 :As_Bb2 :As_Bb3 :As_Bb4 :As_Bb5 :As_Bb6 :As_Bb7     :[]
toNotesBySigPitch B     = B0     :B1     :B2     :B3     :B4     :B5     :B6     :B7         :[]
toNotesBySigPitch C     =         C1     :C2     :C3     :C4     :C5     :C6     :C7     :C8 :[]
toNotesBySigPitch Cs_Db =         Cs_Db1 :Cs_Db2 :Cs_Db3 :Cs_Db4 :Cs_Db5 :Cs_Db6 :Cs_Db7     :[]
toNotesBySigPitch D     =         D1     :D2     :D3     :D4     :D5     :D6     :D7         :[]
toNotesBySigPitch Ds_Eb =         Ds_Eb1 :Ds_Eb2 :Ds_Eb3 :Ds_Eb4 :Ds_Eb5 :Ds_Eb6 :Ds_Eb7     :[]
toNotesBySigPitch E     =         E1     :E2     :E3     :E4     :E5     :E6     :E7         :[]
toNotesBySigPitch F     =         F1     :F2     :F3     :F4     :F5     :F6     :F7         :[]
toNotesBySigPitch Fs_Gb =         Fs_Gb1 :Fs_Gb2 :Fs_Gb3 :Fs_Gb4 :Fs_Gb5 :Fs_Gb6 :Fs_Gb7     :[]
toNotesBySigPitch G     =         G1     :G2     :G3     :G4     :G5     :G6     :G7         :[]
toNotesBySigPitch Gs_Ab =         Gs_Ab1 :Gs_Ab2 :Gs_Ab3 :Gs_Ab4 :Gs_Ab5 :Gs_Ab6 :Gs_Ab7     :[]

--fromOctave is a function that takes an Int (our midi values) and returns a list of YNotes from all piano octaves. 

fromOctave :: Int -> [] YNote
fromOctave 0 =                                            A0:As_Bb0:B0 :[]
fromOctave 1 = C1:Cs_Db1:D1:Ds_Eb1:E1:F1:Fs_Gb1:G1:Gs_Ab1:A1:As_Bb1:B1 :[]
fromOctave 2 = C2:Cs_Db2:D2:Ds_Eb2:E2:F2:Fs_Gb2:G2:Gs_Ab2:A2:As_Bb2:B2 :[]
fromOctave 3 = C3:Cs_Db3:D3:Ds_Eb3:E3:F3:Fs_Gb3:G3:Gs_Ab3:A3:As_Bb3:B3 :[]
fromOctave 4 = C4:Cs_Db4:D4:Ds_Eb4:E4:F4:Fs_Gb4:G4:Gs_Ab4:A4:As_Bb4:B4 :[]
fromOctave 5 = C5:Cs_Db5:D5:Ds_Eb5:E5:F5:Fs_Gb5:G5:Gs_Ab5:A5:As_Bb5:B5 :[]
fromOctave 6 = C6:Cs_Db6:D6:Ds_Eb6:E6:F6:Fs_Gb6:G6:Gs_Ab6:A6:As_Bb6:B6 :[]
fromOctave 7 = C7:Cs_Db7:D7:Ds_Eb7:E7:F7:Fs_Gb7:G7:Gs_Ab7:A7:As_Bb7:B7 :[]
fromOctave 8 = C8                                                      :[]
fromOctave _ = []

--toNoteBySigOctave is a functiont that takes a SigPitch and an Int and retursn a (Maybe YNote). 

toNoteBySigOctave :: SigPitch -> Int -> Maybe YNote
toNoteBySigOctave sp n = if null intersection
                          then Nothing
                          else Just (head intersection)
  where
    fo = fromOctave n
    tn = toNotesBySigPitch sp
    intersection = intersect fo tn

--toOcatve is a function that takes a YNote(each key on the piano) and returns an Int(representing an octave). 

toOctave :: YNote -> Int
toOctave A0     = 0
toOctave As_Bb0 = 0
toOctave B0     = 0
toOctave C1     = 1
toOctave Cs_Db1 = 1
toOctave D1     = 1
toOctave Ds_Eb1 = 1
toOctave E1     = 1
toOctave F1     = 1
toOctave Fs_Gb1 = 1
toOctave G1     = 1
toOctave Gs_Ab1 = 1
toOctave A1     = 1
toOctave As_Bb1 = 1
toOctave B1     = 1
toOctave C2     = 2
toOctave Cs_Db2 = 2
toOctave D2     = 2
toOctave Ds_Eb2 = 2
toOctave E2     = 2
toOctave F2     = 2
toOctave Fs_Gb2 = 2
toOctave G2     = 2
toOctave Gs_Ab2 = 2
toOctave A2     = 2
toOctave As_Bb2 = 2
toOctave B2     = 2
toOctave C3     = 3
toOctave Cs_Db3 = 3
toOctave D3     = 3
toOctave Ds_Eb3 = 3
toOctave E3     = 3
toOctave F3     = 3
toOctave Fs_Gb3 = 3
toOctave G3     = 3
toOctave Gs_Ab3 = 3
toOctave A3     = 3
toOctave As_Bb3 = 3
toOctave B3     = 3
toOctave C4     = 4
toOctave Cs_Db4 = 4
toOctave D4     = 4
toOctave Ds_Eb4 = 4
toOctave E4     = 4
toOctave F4     = 4
toOctave Fs_Gb4 = 4
toOctave G4     = 4
toOctave Gs_Ab4 = 4
toOctave A4     = 4
toOctave As_Bb4 = 4
toOctave B4     = 4
toOctave C5     = 5
toOctave Cs_Db5 = 5
toOctave D5     = 5
toOctave Ds_Eb5 = 5
toOctave E5     = 5
toOctave F5     = 5
toOctave Fs_Gb5 = 5
toOctave G5     = 5
toOctave Gs_Ab5 = 5
toOctave A5     = 5
toOctave As_Bb5 = 5
toOctave B5     = 5
toOctave C6     = 6
toOctave Cs_Db6 = 6
toOctave D6     = 6
toOctave Ds_Eb6 = 6
toOctave E6     = 6
toOctave F6     = 6
toOctave Fs_Gb6 = 6
toOctave G6     = 6
toOctave Gs_Ab6 = 6
toOctave A6     = 6
toOctave As_Bb6 = 6
toOctave B6     = 6
toOctave C7     = 7
toOctave Cs_Db7 = 7
toOctave D7     = 7
toOctave Ds_Eb7 = 7
toOctave E7     = 7
toOctave F7     = 7
toOctave Fs_Gb7 = 7
toOctave G7     = 7
toOctave Gs_Ab7 = 7
toOctave A7     = 7
toOctave As_Bb7 = 7
toOctave B7     = 7
toOctave C8     = 8

--toSigPitch is a function that takes a YNote and returns a SigPitch. This is our converter from YNote to SigPitch. 

toSigPitch :: YNote -> SigPitch
toSigPitch A0     = A 
toSigPitch As_Bb0 = As_Bb 
toSigPitch B0     = B 
toSigPitch C1     = C 
toSigPitch Cs_Db1 = Cs_Db 
toSigPitch D1     = D 
toSigPitch Ds_Eb1 = Ds_Eb 
toSigPitch E1     = E 
toSigPitch F1     = F 
toSigPitch Fs_Gb1 = Fs_Gb 
toSigPitch G1     = G 
toSigPitch Gs_Ab1 = Gs_Ab 
toSigPitch A1     = A 
toSigPitch As_Bb1 = As_Bb 
toSigPitch B1     = B 
toSigPitch C2     = C 
toSigPitch Cs_Db2 = Cs_Db 
toSigPitch D2     = D 
toSigPitch Ds_Eb2 = Ds_Eb 
toSigPitch E2     = E 
toSigPitch F2     = F 
toSigPitch Fs_Gb2 = Fs_Gb 
toSigPitch G2     = G 
toSigPitch Gs_Ab2 = Gs_Ab 
toSigPitch A2     = A 
toSigPitch As_Bb2 = As_Bb 
toSigPitch B2     = B 
toSigPitch C3     = C 
toSigPitch Cs_Db3 = Cs_Db 
toSigPitch D3     = D 
toSigPitch Ds_Eb3 = Ds_Eb 
toSigPitch E3     = E 
toSigPitch F3     = F 
toSigPitch Fs_Gb3 = Fs_Gb 
toSigPitch G3     = G 
toSigPitch Gs_Ab3 = Gs_Ab 
toSigPitch A3     = A 
toSigPitch As_Bb3 = As_Bb 
toSigPitch B3     = B 
toSigPitch C4     = C 
toSigPitch Cs_Db4 = Cs_Db 
toSigPitch D4     = D 
toSigPitch Ds_Eb4 = Ds_Eb 
toSigPitch E4     = E 
toSigPitch F4     = F 
toSigPitch Fs_Gb4 = Fs_Gb 
toSigPitch G4     = G 
toSigPitch Gs_Ab4 = Gs_Ab 
toSigPitch A4     = A 
toSigPitch As_Bb4 = As_Bb 
toSigPitch B4     = B 
toSigPitch C5     = C 
toSigPitch Cs_Db5 = Cs_Db 
toSigPitch D5     = D 
toSigPitch Ds_Eb5 = Ds_Eb 
toSigPitch E5     = E 
toSigPitch F5     = F 
toSigPitch Fs_Gb5 = Fs_Gb 
toSigPitch G5     = G 
toSigPitch Gs_Ab5 = Gs_Ab 
toSigPitch A5     = A 
toSigPitch As_Bb5 = As_Bb 
toSigPitch B5     = B 
toSigPitch C6     = C 
toSigPitch Cs_Db6 = Cs_Db 
toSigPitch D6     = D 
toSigPitch Ds_Eb6 = Ds_Eb 
toSigPitch E6     = E 
toSigPitch F6     = F 
toSigPitch Fs_Gb6 = Fs_Gb 
toSigPitch G6     = G 
toSigPitch Gs_Ab6 = Gs_Ab 
toSigPitch A6     = A 
toSigPitch As_Bb6 = As_Bb 
toSigPitch B6     = B 
toSigPitch C7     = C 
toSigPitch Cs_Db7 = Cs_Db 
toSigPitch D7     = D 
toSigPitch Ds_Eb7 = Ds_Eb 
toSigPitch E7     = E 
toSigPitch F7     = F 
toSigPitch Fs_Gb7 = Fs_Gb 
toSigPitch G7     = G 
toSigPitch Gs_Ab7 = Gs_Ab 
toSigPitch A7     = A 
toSigPitch As_Bb7 = As_Bb 
toSigPitch B7     = B 
toSigPitch C8     = C 

-- toMidi is a function that takes a YNote and returns an Int (midi value). Now we can convert from a YNote to an Int representing a midi value. 

toMidi :: YNote -> Int
toMidi A0     = 21
toMidi As_Bb0 = 22
toMidi B0     = 23
toMidi C1     = 24
toMidi Cs_Db1 = 25
toMidi D1     = 26
toMidi Ds_Eb1 = 27
toMidi E1     = 28
toMidi F1     = 29
toMidi Fs_Gb1 = 30
toMidi G1     = 31
toMidi Gs_Ab1 = 32
toMidi A1     = 33
toMidi As_Bb1 = 34
toMidi B1     = 35
toMidi C2     = 36
toMidi Cs_Db2 = 37
toMidi D2     = 38
toMidi Ds_Eb2 = 39
toMidi E2     = 40
toMidi F2     = 41
toMidi Fs_Gb2 = 42
toMidi G2     = 43
toMidi Gs_Ab2 = 44
toMidi A2     = 45
toMidi As_Bb2 = 46
toMidi B2     = 47
toMidi C3     = 48
toMidi Cs_Db3 = 49
toMidi D3     = 50
toMidi Ds_Eb3 = 51
toMidi E3     = 52
toMidi F3     = 53
toMidi Fs_Gb3 = 54
toMidi G3     = 55
toMidi Gs_Ab3 = 56
toMidi A3     = 57
toMidi As_Bb3 = 58
toMidi B3     = 59
toMidi C4     = 60
toMidi Cs_Db4 = 61
toMidi D4     = 62
toMidi Ds_Eb4 = 63
toMidi E4     = 64
toMidi F4     = 65
toMidi Fs_Gb4 = 66
toMidi G4     = 67
toMidi Gs_Ab4 = 68
toMidi A4     = 69
toMidi As_Bb4 = 70
toMidi B4     = 71
toMidi C5     = 72
toMidi Cs_Db5 = 73
toMidi D5     = 74
toMidi Ds_Eb5 = 75
toMidi E5     = 76
toMidi F5     = 77
toMidi Fs_Gb5 = 78
toMidi G5     = 79
toMidi Gs_Ab5 = 80
toMidi A5     = 81
toMidi As_Bb5 = 82
toMidi B5     = 83
toMidi C6     = 84
toMidi Cs_Db6 = 85
toMidi D6     = 86
toMidi Ds_Eb6 = 87
toMidi E6     = 88
toMidi F6     = 89
toMidi Fs_Gb6 = 90
toMidi G6     = 91
toMidi Gs_Ab6 = 92
toMidi A6     = 93
toMidi As_Bb6 = 94
toMidi B6     = 95
toMidi C7     = 96
toMidi Cs_Db7 = 97
toMidi D7     = 98
toMidi Ds_Eb7 = 99
toMidi E7     = 100
toMidi F7     = 101
toMidi Fs_Gb7 = 102
toMidi G7     = 103
toMidi Gs_Ab7 = 104
toMidi A7     = 105
toMidi As_Bb7 = 106
toMidi B7     = 107
toMidi C8     = 108

-- fromMidi: takes an Int returning a (Maybe YNote).
--           It converts from midi values to YNotes with corresponding pitch and octave.

fromMidi :: Int -> Maybe YNote
fromMidi 21  = Just A0
fromMidi 22  = Just As_Bb0
fromMidi 23  = Just B0
fromMidi 24  = Just C1
fromMidi 25  = Just Cs_Db1
fromMidi 26  = Just D1
fromMidi 27  = Just Ds_Eb1
fromMidi 28  = Just E1
fromMidi 29  = Just F1
fromMidi 30  = Just Fs_Gb1
fromMidi 31  = Just G1
fromMidi 32  = Just Gs_Ab1
fromMidi 33  = Just A1
fromMidi 34  = Just As_Bb1
fromMidi 35  = Just B1
fromMidi 36  = Just C2
fromMidi 37  = Just Cs_Db2
fromMidi 38  = Just D2
fromMidi 39  = Just Ds_Eb2
fromMidi 40  = Just E2
fromMidi 41  = Just F2
fromMidi 42  = Just Fs_Gb2
fromMidi 43  = Just G2
fromMidi 44  = Just Gs_Ab2
fromMidi 45  = Just A2
fromMidi 46  = Just As_Bb2
fromMidi 47  = Just B2
fromMidi 48  = Just C3
fromMidi 49  = Just Cs_Db3
fromMidi 50  = Just D3
fromMidi 51  = Just Ds_Eb3
fromMidi 52  = Just E3
fromMidi 53  = Just F3
fromMidi 54  = Just Fs_Gb3
fromMidi 55  = Just G3
fromMidi 56  = Just Gs_Ab3
fromMidi 57  = Just A3
fromMidi 58  = Just As_Bb3
fromMidi 59  = Just B3
fromMidi 60  = Just C4
fromMidi 61  = Just Cs_Db4
fromMidi 62  = Just D4
fromMidi 63  = Just Ds_Eb4
fromMidi 64  = Just E4
fromMidi 65  = Just F4
fromMidi 66  = Just Fs_Gb4
fromMidi 67  = Just G4
fromMidi 68  = Just Gs_Ab4
fromMidi 69  = Just A4
fromMidi 70  = Just As_Bb4
fromMidi 71  = Just B4
fromMidi 72  = Just C5
fromMidi 73  = Just Cs_Db5
fromMidi 74  = Just D5
fromMidi 75  = Just Ds_Eb5
fromMidi 76  = Just E5
fromMidi 77  = Just F5
fromMidi 78  = Just Fs_Gb5
fromMidi 79  = Just G5
fromMidi 80  = Just Gs_Ab5
fromMidi 81  = Just A5
fromMidi 82  = Just As_Bb5
fromMidi 83  = Just B5
fromMidi 84  = Just C6
fromMidi 85  = Just Cs_Db6
fromMidi 86  = Just D6
fromMidi 87  = Just Ds_Eb6
fromMidi 88  = Just E6
fromMidi 89  = Just F6
fromMidi 90  = Just Fs_Gb6
fromMidi 91  = Just G6
fromMidi 92  = Just Gs_Ab6
fromMidi 93  = Just A6
fromMidi 94  = Just As_Bb6
fromMidi 95  = Just B6
fromMidi 96  = Just C7
fromMidi 97  = Just Cs_Db7
fromMidi 98  = Just D7
fromMidi 99  = Just Ds_Eb7
fromMidi 100 = Just E7
fromMidi 101 = Just F7
fromMidi 102 = Just Fs_Gb7
fromMidi 103 = Just G7
fromMidi 104 = Just Gs_Ab7
fromMidi 105 = Just A7
fromMidi 106 = Just As_Bb7
fromMidi 107 = Just B7
fromMidi 108 = Just C8
fromMidi _   = Nothing

type YMelody = [] YNote

type Note = Int
type Melody = [Note]
type MidiEvent = (Ticks, Message)


midiEmpty :: Track Ticks -> Midi
midiEmpty  crush  =  Midi {
         fileType = MultiTrack, 
         timeDiv = TicksPerBeat 480, 
         tracks = [
          [
           (0,ChannelPrefix 0),
           (0,TrackName " Grand Piano  "),
           (0,InstrumentName "GM Device  1"),
           (0,TimeSignature 4 2 24 8),
           (0,KeySignature 0 0)
          ]
          ++
          crush
          ++
          [
           (0,TrackEnd)
          ]
         ]
       }  


keydown :: Note -> MidiEvent
keydown k =  (0,NoteOn {channel = 0, key = k, velocity = 80})

keyup :: Note -> MidiEvent
keyup k =  (480,NoteOn {channel = 0, key = k, velocity = 0})

piggynote :: Note -> Track Ticks
piggynote k = [ keydown k, keyup k]
 

magicMidi :: FilePath -> Melody -> IO()
magicMidi f notes = exportFile  f $ midiEmpty  $ concat $ map  piggynote notes

yusicMidi :: FilePath -> YMelody -> IO()
yusicMidi fileName melody = magicMidi fileName $ map toMidi melody

sampleMelody :: YMelody
sampleMelody = [C3, B2, F1]


-- to test: yusicMidi "foo.mid" sampleMelody
