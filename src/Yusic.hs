{-# LANGUAGE OverloadedStrings #-}

module Yusic
where

import qualified Data.Text as T
import Data.List

yusic :: T.Text
yusic = "Well Hello Dolly"


--Super All Notes = [A0,As0_Bb0,C1,Cs1_Db1,D1,Ds1_Eb1,E1,F1,Fs1_Gb1,G1,Gs1_Ab1,A1,As1_Bb1,B1,C2,Cs2_Db2,D2,Ds2_Eb2,E2,F2,Fs2_Gb2,G,Gs2_Ab2,A2,As2_Bb2,B2,C3,Cs3_Db3,D3,Ds3_Eb3,E3,F3,Fs3_Gb3,G3,Gs3_Ab3,A3,As3_Bb3,B3,C4,Cs4_Db4,D4,Ds4_Eb4,E4,F4,Fs4_Gb4,G4,Gs4_Ab4,A4,As4_Bb4,B4,C5,Cs5_Db5,D5,Ds5_Eb5,E5,F5,Fs5_Gb5,G5,Gs5_Ab5,A5,As5_Bb5,B5,C6,Cs6_Db6,D6,Ds6_Eb6,E6,F6,Fs6_Gb6,G6,Gs6_Ab6,A6,As6_Bb6,B6,C7,Cs7_Db7,D7,Ds7_Eb7,E7,F7,Fs7_Gb7,G7,Gs7_Ab7,A7,As7_Bb7,B7,C8]
--Super Midi Notes = [21,22,23,24,25,26,27,28,29,30,31,32,33,34,35,36,37,38,39,40,41,42,43,44,45,46,47,48,49,50,51,52,53,54,55,56,57,58,59,60,61,62,63,64,65,66,67,68,69,70,71,72,73,74,75,76,77,78,79,80,81,82,83,84,85,86,87,88,89,90,91,92,93,94,95,96,97,98,99,100,101,102,103,104,105,106,107,108]

listNum :: [] Int
listNum = [21,22,23,24,25,26,27,28,29,30,31,32,33,34,35,36,37,38,39,40,41,42,43,44,45,46,47,48,49,50,51,52,53,54,55,56,57,58,59,60,61,62,63,64,65,66,67,68,69,70,71,72,73,74,75,76,77,78,79,80,81,82,83,84,85,86,87,88,89,90,91,92,93,94,95,96,97,98,99,100,101,102,103,104,105,106,107,108]

listNotes :: [] Notes
listNotes = [A0,As0_Bb0,B0,C1,Cs1_Db1,D1,Ds1_Eb1,E1,F1,Fs1_Gb1,G1,Gs1_Ab1,A1,As1_Bb1,B1,C2,Cs2_Db2,D2,Ds2_Eb2,E2,F2,Fs2_Gb2,G2,Gs2_Ab2,A2,As2_Bb2,B2,C3,Cs3_Db3,D3,Ds3_Eb3,E3,F3,Fs3_Gb3,G3,Gs3_Ab3,A3,As3_Bb3,B3,C4,Cs4_Db4,D4,Ds4_Eb4,E4,F4,Fs4_Gb4,G4,Gs4_Ab4,A4,As4_Bb4,B4,C5,Cs5_Db5,D5,Ds5_Eb5,E5,F5,Fs5_Gb5,G5,Gs5_Ab5,A5,As5_Bb5,B5,C6,Cs6_Db6,D6,Ds6_Eb6,E6,F6,Fs6_Gb6,G6,Gs6_Ab6,A6,As6_Bb6,B6,C7,Cs7_Db7,D7,Ds7_Eb7,E7,F7,Fs7_Gb7,G7,Gs7_Ab7,A7,As7_Bb7,B7,C8]


data Notes = A0
           | As0_Bb0
           | C1
           | Cs1_Db1
           | Ds1_Eb1
           | E1
           | F1
           | Fs1_Gb1
           | G1
           | Gs1_Ab1
           | A1
           | As1_Bb1
           | B1
           | C2
           | Cs2_Db2
           | Ds2_Eb2
           | E2
           | F2
           | Fs2_Gb2
           | G2
           | Gs2_Ab2
           | A2
           | As2_Bb2
           | B2
           | C3
           | Cs3_Db3
           | Ds3_Eb3
           | E3
           | F3
           | Fs3_Gb3
           | G3
           | Gs3_Ab3
           | A3
           | As3_Bb3
           | B3
           | C4
           | Cs4_Db4
           | Ds4_Eb4
           | E4
           | F4
           | Fs4_Gb4
           | G4
           | Gs4_Ab4
           | A4
           | As4_Bb4
           | B4
           | C5
           | Cs5_Db5
           | Ds5_Eb5
           | E5
           | F5
           | Fs5_Gb5
           | G5
           | Gs5_Ab5
           | A5
           | As5_Bb5
           | B5
           | C6
           | Cs6_Db6
           | Ds6_Eb6
           | E6
           | F6
           | Fs6_Gb6
           | G6
           | Gs6_Ab6
           | A6
           | As6_Bb6
           | B6
           | C7
           | Cs7_Db7
           | Ds7_Eb7
           | E7
           | F7
           | Fs7_Gb7
           | G7
           | Gs7_Ab7
           | A7
           | As7_Bb7
           | B7
           | C8
             deriving (Show, Eq, Ord, Enum)

toOctave :: Notes -> Int
toOctave A0      = 0
toOctave As0_Bb0 = 0
toOctave C1      = 1
toOctave Cs1_Db1 = 1
toOctave Ds1_Eb1 = 1
toOctave E1      = 1
toOctave F1      = 1
toOctave Fs1_Gb1 = 1
toOctave G1      = 1
toOctave Gs1_Ab1 = 1
toOctave A1      = 1
toOctave As1_Bb1 = 1
toOctave B1      = 1
toOctave C2      = 2
toOctave Cs2_Db2 = 2
toOctave Ds2_Eb2 = 2
toOctave E2      = 2
toOctave F2      = 2
toOctave Fs2_Gb2 = 2
toOctave G2      = 2
toOctave Gs2_Ab2 = 2
toOctave A2      = 2
toOctave As2_Bb2 = 2
toOctave B2      = 2
toOctave C3      = 3
toOctave Cs3_Db3 = 3
toOctave Ds3_Eb3 = 3
toOctave E3      = 3
toOctave F3      = 3
toOctave Fs3_Gb3 = 3
toOctave G3      = 3
toOctave Gs3_Ab3 = 3
toOctave A3      = 3
toOctave As3_Bb3 = 3
toOctave B3      = 3
toOctave C4      = 4
toOctave Cs4_Db4 = 4
toOctave Ds4_Eb4 = 4
toOctave E4      = 4
toOctave F4      = 4
toOctave Fs4_Gb4 = 4
toOctave G4      = 4
toOctave Gs4_Ab4 = 4
toOctave A4      = 4
toOctave As4_Bb4 = 4
toOctave B4      = 4
toOctave C5      = 5
toOctave Cs5_Db5 = 5
toOctave Ds5_Eb5 = 5
toOctave E5      = 5
toOctave F5      = 5
toOctave Fs5_Gb5 = 5
toOctave G5      = 5
toOctave Gs5_Ab5 = 5
toOctave A5      = 5
toOctave As5_Bb5 = 5
toOctave B5      = 5
toOctave C6      = 6
toOctave Cs6_Db6 = 6
toOctave Ds6_Eb6 = 6
toOctave E6      = 6
toOctave F6      = 6
toOctave Fs6_Gb6 = 6
toOctave G6      = 6
toOctave Gs6_Ab6 = 6
toOctave A6      = 6
toOctave As6_Bb6 = 6
toOctave B6      = 6
toOctave C7      = 7
toOctave Cs7_Db7 = 7
toOctave Ds7_Eb7 = 7
toOctave E7      = 7
toOctave F7      = 7
toOctave Fs7_Gb7 = 7
toOctave G7      = 7
toOctave Gs7_Ab7 = 7
toOctave A7      = 7
toOctave As7_Bb7 = 7
toOctave B7      = 7
toOctave C8      = 8





data SigPitch = C
     	      | Cs_Db
              | D
	      | Ds_Eb
              | E
              | F
	      | Fs_Gb
              | G
	      | Gs_Ab
              | A
	      | As_Bb
              | B
                deriving (Show, Ord, Eq) -- You can now perform math ops on SigPitch (e.g. (As < B), (C /= D), (G == G), etc.)
                                         -- Which means you can  sort [E, G, C] = [C,E,G] (same as C:E:G:[]) inside toCanonicalSignature
                                         -- and order is correct.

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

              | KG_Cs_DbMajor               -- Begin Cs/Db
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
                deriving (Show)

toKeyGuide :: [SigPitch] -> Maybe KeyGuide                                        -- Account for broken [SigPitch]
toKeyGuide [] = Nothing
toKeyGuide sp | sp `sEQ` [C,E,G                     ] = Just KG_CMajor            -- sort/compare both deconstructor and positional parameter
                        | sp `sEQ` [C,Ds_Eb,G                 ] = Just KG_CMinor
                        | sp `sEQ` [C,G                       ] = Just KG_C5
                        | sp `sEQ` [C,E,G,As_Bb               ] = Just KG_CDominant7
                        | sp `sEQ` [C,E,G,B                   ] = Just KG_CMajor7
                        | sp `sEQ` [C,Ds_Eb,G,As_Bb           ] = Just KG_CMinor7
                        | sp `sEQ` [C,Ds_Eb,G,B               ] = Just KG_CMinorMajor7
                        | sp `sEQ` [C,F,G                     ] = Just KG_CSus4
                        | sp `sEQ` [C,D,G                     ] = Just KG_CSus2
                        | sp `sEQ` [C,E,G,A                   ] = Just KG_C6
                        | sp `sEQ` [C,Ds_Eb,G,A               ] = Just KG_CMinor6
                        | sp `sEQ` [C,E,G,As_Bb               ] = Just KG_C9

                        | sp `sEQ` [Cs_Db,F,Gs_Ab             ] = Just KG_Cs_DbMajor        -- Begin Cs_DbMajor
                        | sp `sEQ` [Cs_Db,E,Gs_Ab             ] = Just KG_Cs_DbMinor
                        | sp `sEQ` [Cs_Db,Gs_Ab               ] = Just KG_Cs_Db5
                        | sp `sEQ` [Cs_Db,G,Gs_Ab,B           ] = Just KG_Cs_DbDominant7
                        | sp `sEQ` [Cs_Db,F,Gs_Ab,C           ] = Just KG_Cs_DbMajor7
                        | sp `sEQ` [Cs_Db,E,Gs_Ab,B           ] = Just KG_Cs_DbMinor7
                        | sp `sEQ` [Cs_Db,E,Gs_Ab,C           ] = Just KG_Cs_DbMinorMajor7
                        | sp `sEQ` [Cs_Db,Fs_Gb,Gs_Ab         ] = Just KG_Cs_DbSus4
                        | sp `sEQ` [Cs_Db,Ds_Eb,Gs_Ab         ] = Just KG_Cs_DbSus2
                        | sp `sEQ` [Cs_Db,F,Gs_Ab,As_Bb       ] = Just KG_Cs_Db6
                        | sp `sEQ` [Cs_Db,E,Gs_Ab,As_Bb       ] = Just KG_Cs_DbMinor6
                        | sp `sEQ` [Cs_Db,Ds_Eb,F,Gs_Ab,B     ] = Just KG_Cs_Db9            -- end Cs_Db

                        | sp `sEQ` [D,Fs_Gb,A                 ] = Just KG_DMajor            -- Begin D
                        | sp `sEQ` [D,F,A                     ] = Just KG_DMinor
                        | sp `sEQ` [D,A                       ] = Just KG_D5
                        | sp `sEQ` [D,Fs_Gb,A,C               ] = Just KG_DDominant7
                        | sp `sEQ` [D,Fs_Gb,A,Cs_Db           ] = Just KG_DMajor7
                        | sp `sEQ` [D,F,A,C                   ] = Just KG_DMinor7
                        | sp `sEQ` [D,F,G,Cs_Db               ] = Just KG_DMinorMajor7
                        | sp `sEQ` [D,G,A                     ] = Just KG_DSus4
                        | sp `sEQ` [D,E,A                     ] = Just KG_DSus2
                        | sp `sEQ` [D,Fs_Gb,A,B               ] = Just KG_D6
                        | sp `sEQ` [D,F,A,B                   ] = Just KG_DMinor6
                        | sp `sEQ` [D,E,Fs_Gb,A,C             ] = Just KG_D9                -- end DMajor

                        | sp `sEQ` [Ds_Eb,G,As_Bb             ] = Just KG_Ds_EbMajor        -- Begin Ds_Eb
                        | sp `sEQ` [Ds_Eb,Fs_Gb,As_Bb         ] = Just KG_Ds_EbMinor
                        | sp `sEQ` [Ds_Eb,As_Bb               ] = Just KG_Ds_Eb5
                        | sp `sEQ` [Ds_Eb,G,As_Bb,Cs_Db       ] = Just KG_Ds_EbDominant7
                        | sp `sEQ` [Ds_Eb,G,As_Bb,D           ] = Just KG_Ds_EbMajor7
                        | sp `sEQ` [Ds_Eb,Fs_Gb,As_Bb,Cs_Db   ] = Just KG_Ds_EbMinor7
                        | sp `sEQ` [Ds_Eb,Fs_Gb,As_Bb,D       ] = Just KG_Ds_EbMinorMajor7
                        | sp `sEQ` [Ds_Eb,Gs_Ab,As_Bb         ] = Just KG_Ds_EbSus4
                        | sp `sEQ` [Ds_Eb,F,As_Bb             ] = Just KG_Ds_EbSus2
                        | sp `sEQ` [Ds_Eb,G,As_Bb,C           ] = Just KG_Ds_Eb6
                        | sp `sEQ` [Ds_Eb,Fs_Gb,As_Bb,C       ] = Just KG_Ds_EbMinor6
                        | sp `sEQ` [Ds_Eb,F,G,As_Bb,Cs_Db     ] = Just KG_Ds_Eb9            -- end Ds_Eb

                        | sp `sEQ` [E,Gs_Ab,B                 ] = Just KG_EMajor            -- Begin E
                        | sp `sEQ` [E,G,B                     ] = Just KG_EMinor
                        | sp `sEQ` [E,B                       ] = Just KG_E5
                        | sp `sEQ` [E,Gs_Ab,B,D               ] = Just KG_EDominant7
                        | sp `sEQ` [E,Gs_Ab,B,Ds_Eb           ] = Just KG_EMajor7
                        | sp `sEQ` [E,G,B,D                   ] = Just KG_EMinor7
                        | sp `sEQ` [E,G,B,Cs_Db               ] = Just KG_EMinorMajor7
                        | sp `sEQ` [E,A,B                     ] = Just KG_ESus4
                        | sp `sEQ` [E,Fs_Gb,B                 ] = Just KG_ESus2
                        | sp `sEQ` [E,Gs_Ab,B,Cs_Db           ] = Just KG_E6
                        | sp `sEQ` [E,G,B,Cs_Db               ] = Just KG_EMinor6
                        | sp `sEQ` [E,Fs_Gb,Gs_Ab,B,D         ] = Just KG_E9                -- end E

                        | sp `sEQ` [F,A,C                     ] = Just KG_F_EsMajor         -- Begin F_Es
                        | sp `sEQ` [F,Gs_Ab,C                 ] = Just KG_F_EsMinor
                        | sp `sEQ` [F,C                       ] = Just KG_F_Es5
                        | sp `sEQ` [F,G,C,Ds_Eb               ] = Just KG_F_EsDominant7
                        | sp `sEQ` [F,A,C,E                   ] = Just KG_F_EsMajor7
                        | sp `sEQ` [F,Gs_Ab,C,Ds_Eb           ] = Just KG_F_EsMinor7
                        | sp `sEQ` [F,Gs_Ab,C,E               ] = Just KG_F_EsMinorMajor7
                        | sp `sEQ` [F,As_Bb,C                 ] = Just KG_F_EsSus4
                        | sp `sEQ` [F,G,C                     ] = Just KG_F_EsSus2
                        | sp `sEQ` [F,A,C,D                   ] = Just KG_F_Es6
                        | sp `sEQ` [F,Gs_Ab,C,D               ] = Just KG_F_EsMinor6
                        | sp `sEQ` [F,G,A,C,Ds_Eb             ] = Just KG_F_Es9             -- end F_Es

                        | sp `sEQ` [Fs_Gb,As_Bb,Cs_Db         ] = Just KG_Fs_GbMajor        -- Begin Fs_Gb
                        | sp `sEQ` [Fs_Gb,A,Cs_Db             ] = Just KG_Fs_GbMinor
                        | sp `sEQ` [Fs_Gb,Cs_Db               ] = Just KG_Fs_Gb5
                        | sp `sEQ` [Fs_Gb,Fs_Gb,As_Bb,E       ] = Just KG_Fs_GbDominant7
                        | sp `sEQ` [Fs_Gb,As_Bb,F             ] = Just KG_Fs_GbMajor7
                        | sp `sEQ` [Fs_Gb,A,Cs_Db,E           ] = Just KG_Fs_GbMinor7
                        | sp `sEQ` [Fs_Gb,A,Cs_Db,F           ] = Just KG_Fs_GbMinorMajor7
                        | sp `sEQ` [Fs_Gb,B,Cs_Db             ] = Just KG_Fs_GbSus4
                        | sp `sEQ` [Fs_Gb,Gs_Ab,Cs_Db         ] = Just KG_Fs_GbSus2
                        | sp `sEQ` [Fs_Gb,As_Bb,Cs_Db,Ds_Eb   ] = Just KG_Fs_Gb6
                        | sp `sEQ` [Fs_Gb,A,Cs_Db,Ds_Eb       ] = Just KG_Fs_GbMinor6
                        | sp `sEQ` [Fs_Gb,Gs_Ab,As_Bb,E       ] = Just KG_Fs_Gb9            --end Fs_Gb

                        | sp `sEQ` [G,B,D                     ] = Just KG_GMajor            --Begin G
                        | sp `sEQ` [G,As_Bb,D                 ] = Just KG_GMinor
                        | sp `sEQ` [G,D                       ] = Just KG_G5
                        | sp `sEQ` [G,B,D,F                   ] = Just KG_GDominant7
                        | sp `sEQ` [G,B,D,Fs_Gb               ] = Just KG_GMajor7
                        | sp `sEQ` [G,As_Bb,D,F               ] = Just KG_GMinor7
                        | sp `sEQ` [G,As_Bb,D,Fs_Gb           ] = Just KG_GMinorMajor7
                        | sp `sEQ` [G,C,D                     ] = Just KG_GSus4
                        | sp `sEQ` [G,A,D                     ] = Just KG_GSus2
                        | sp `sEQ` [G,B,D,E                   ] = Just KG_G6
                        | sp `sEQ` [G,As_Bb,D,E               ] = Just KG_Gs_AbMinor6
                        | sp `sEQ` [G,A,B,D,F                 ] = Just KG_Gs_Ab9            -- end G

                        | sp `sEQ` [Gs_Ab,C,Ds_Eb             ] = Just KG_Gs_AbMajor        -- Begin Gs_Ab
                        | sp `sEQ` [Gs_Ab,B,Ds_Eb             ] = Just KG_Gs_AbMinor
                        | sp `sEQ` [Gs_Ab,Ds_Eb               ] = Just KG_Gs_Ab5
                        | sp `sEQ` [Gs_Ab,C,Ds_Eb,Cs_Db       ] = Just KG_Gs_AbDominant7
                        | sp `sEQ` [Gs_Ab,C,Ds_Eb,G           ] = Just KG_Gs_AbMajor7
                        | sp `sEQ` [Gs_Ab,B,Ds_Eb,Cs_Db       ] = Just KG_Gs_AbMinor7
                        | sp `sEQ` [Gs_Ab,B,Ds_Eb,G           ] = Just KG_Gs_AbMinorMajor7
                        | sp `sEQ` [Gs_Ab,Cs_Db,Ds_Eb         ] = Just KG_Gs_AbSus4
                        | sp `sEQ` [Gs_Ab,As_Bb,Ds_Eb         ] = Just KG_Gs_AbSus2
                        | sp `sEQ` [Gs_Ab,C,Ds_Eb,F           ] = Just KG_Gs_Ab6
                        | sp `sEQ` [Gs_Ab,B,Ds_Eb,F           ] = Just KG_Gs_AbMinor6
                        | sp `sEQ` [Gs_Ab,As_Bb,C,Ds_Eb,Fs_Gb ] = Just KG_Gs_Ab9            --end Gs_Ab

                        | sp `sEQ` [A,Cs_Db,E                 ] = Just KG_AMajor            --Begin A
                        | sp `sEQ` [A,C,E                     ] = Just KG_AMinor
                        | sp `sEQ` [A,E                       ] = Just KG_A5
                        | sp `sEQ` [A,Cs_Db,E,G               ] = Just KG_ADominant7
                        | sp `sEQ` [A,Cs_Db,E,Gs_Ab           ] = Just KG_AMajor7
                        | sp `sEQ` [A,C,E,G                   ] = Just KG_AMinor7
                        | sp `sEQ` [A,C,E,Gs_Ab               ] = Just KG_AMinorMajor7
                        | sp `sEQ` [A,D,E                     ] = Just KG_ASus4
                        | sp `sEQ` [A,B,E                     ] = Just KG_ASus2
                        | sp `sEQ` [A,Cs_Db,Fs_Gb             ] = Just KG_A6
                        | sp `sEQ` [A,C,E,Fs_Gb               ] = Just KG_AMinor6
                        | sp `sEQ` [A,B,Cs_Db,E,G             ] = Just KG_A9                -- end A

                        | sp `sEQ` [As_Bb,D,F                 ] = Just KG_As_BbMajor        -- Begin As_Bb
                        | sp `sEQ` [As_Bb,Cs_Db,F             ] = Just KG_As_BbMinor
                        | sp `sEQ` [As_Bb,F                   ] = Just KG_As_Bb5
                        | sp `sEQ` [As_Bb,D,F,Gs_Ab           ] = Just KG_As_BbDominant7
                        | sp `sEQ` [As_Bb,D,F,A               ] = Just KG_As_BbMajor7
                        | sp `sEQ` [As_Bb,Cs_Db,F,Gs_Ab       ] = Just KG_As_BbMinor7
                        | sp `sEQ` [As_Bb,Cs_Db,F,A           ] = Just KG_As_BbMinorMajor7
                        | sp `sEQ` [As_Bb,Ds_Eb,F             ] = Just KG_As_BbSus4
                        | sp `sEQ` [As_Bb,C,F                 ] = Just KG_As_BbSus2
                        | sp `sEQ` [As_Bb,D,F,G               ] = Just KG_As_Bb6
                        | sp `sEQ` [As_Bb,Cs_Db,F,G           ] = Just KG_As_BbMinor6
                        | sp `sEQ` [As_Bb,C,D,F,Gs_Ab         ] = Just KG_As_Bb9            -- end As_Bb

                        | sp `sEQ` [B,Ds_Eb,Fs_Gb             ] = Just KG_B_CbMajor         -- Begin B_Cb
                        | sp `sEQ` [B,D,Fs_Gb                 ] = Just KG_B_CbMinor
                        | sp `sEQ` [B,Fs_Gb                   ] = Just KG_B_Cb5
                        | sp `sEQ` [B,Ds_Eb,Fs_Gb,A           ] = Just KG_B_CbDominant7
                        | sp `sEQ` [B,Ds_Eb,Fs_Gb,As_Bb       ] = Just KG_B_CbMajor7
                        | sp `sEQ` [B,D,Fs_Gb,A               ] = Just KG_B_CbMinor7
                        | sp `sEQ` [B,D,Fs_Gb,As_Bb           ] = Just KG_B_CbMinorMajor7
                        | sp `sEQ` [B,E,Fs_Gb                 ] = Just KG_B_CbSus4
                        | sp `sEQ` [B,Cs_Db,Fs_Gb             ] = Just KG_B_CbSus2
                        | sp `sEQ` [B,Ds_Eb,Fs_Gb,Gs_Ab       ] = Just KG_B_Cb6
                        | sp `sEQ` [B,D,Fs_Gb,Gs_Ab           ] = Just KG_B_CbMinor6
                        | sp `sEQ` [B,Cs_Db,Ds_Eb,Fs_Gb,A     ] = Just KG_B_Cb9             -- end B_Cb
                        | otherwise                             = Nothing                -- Broken [SigPitch] deserves Nothing !!!

  where
    sEQ :: (Eq a, Ord a) => [a] -> [a] -> Bool    -- Sort and compare two SigPitch lists
    sEQ a b = sort a == sort b

fromKeyGuide :: KeyGuide -> [SigPitch]
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
fromKeyGuide KG_EMinorMajor7     = E:G:B:Cs_Db               :[]
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
fromKeyGuide KG_Fs_GbDominant7   = Fs_Gb:Fs_Gb:As_Bb:E       :[]
fromKeyGuide KG_Fs_GbMajor7      = Fs_Gb:As_Bb:F             :[]
fromKeyGuide KG_Fs_GbMinor7      = Fs_Gb:A:Cs_Db:E           :[]
fromKeyGuide KG_Fs_GbMinorMajor7 = Fs_Gb:A:Cs_Db:F           :[]
fromKeyGuide KG_Fs_GbSus4        = Fs_Gb:B:Cs_Db             :[]
fromKeyGuide KG_Fs_GbSus2        = Fs_Gb:Gs_Ab:Cs_Db         :[]
fromKeyGuide KG_Fs_Gb6           = Fs_Gb:As_Bb:Cs_Db:Ds_Eb   :[]
fromKeyGuide KG_Fs_GbMinor6      = Fs_Gb:A:Cs_Db:Ds_Eb       :[]
fromKeyGuide KG_Fs_Gb9           = Fs_Gb:Gs_Ab:As_Bb:E       :[] -- End Fs_Gb

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
