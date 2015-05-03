{-# LANGUAGE OverloadedStrings #-}

module Yusic
where

import qualified Data.Text as T
import Data.List

yusic :: T.Text
yusic = "Well Hello Dolly"

data SigPitch = C | Cs_Db   | D  | Ds_Eb   | E  | F  | Fs_Gb   | G  | Gs_Ab   | A  | As_Bb   | B  deriving (Show, Ord, Eq)

data Note =                                                                     A0 | As_Bb_0 | B0
             | C1 | Cs_Db_1 | D1 | Ds_Eb_1 | E1 | F1 | Fs_Gb_1 | G1 | Gs_Ab_1 | A1 | As_Bb_1 | B1
             | C2 | Cs_Db_2 | D2 | Ds_Eb_2 | E2 | F2 | Fs_Gb_2 | G2 | Gs_Ab_2 | A2 | As_Bb_2 | B2
             | C3 | Cs_Db_3 | D3 | Ds_Eb_3 | E3 | F3 | Fs_Gb_3 | G3 | Gs_Ab_3 | A3 | As_Bb_3 | B3
             | C4 | Cs_Db_4 | D4 | Ds_Eb_4 | E4 | F4 | Fs_Gb_4 | G4 | Gs_Ab_4 | A4 | As_Bb_4 | B4
             | C5 | Cs_Db_5 | D5 | Ds_Eb_5 | E5 | F5 | Fs_Gb_5 | G5 | Gs_Ab_5 | A5 | As_Bb_5 | B5
             | C6 | Cs_Db_6 | D6 | Ds_Eb_6 | E6 | F6 | Fs_Gb_6 | G6 | Gs_Ab_6 | A6 | As_Bb_6 | B6
             | C7 | Cs_Db_7 | D7 | Ds_Eb_7 | E7 | F7 | Fs_Gb_7 | G7 | Gs_Ab_7 | A7 | As_Bb_7 | B7
             | C8
               deriving (Show, Eq, Ord, Enum)

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
                deriving (Show)

toKeyGuide :: [SigPitch] -> Maybe KeyGuide
toKeyGuide [] = Nothing
toKeyGuide sp | sp `sEQ` [C,E,G                     ] = Just KG_CMajor     -- sort/compare both deconstructor and positional parameter
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
                                                        
              | sp `sEQ` [Cs_Db,F,Gs_Ab             ] = Just KG_Cs_DbMajor -- Begin Cs_DbMajor
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
              | sp `sEQ` [Cs_Db,Ds_Eb,F,Gs_Ab,B     ] = Just KG_Cs_Db9     -- End Cs_Db
                                                        
              | sp `sEQ` [D,Fs_Gb,A                 ] = Just KG_DMajor     -- Begin D
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
              | sp `sEQ` [D,E,Fs_Gb,A,C             ] = Just KG_D9         -- End DMajor
                                                        
              | sp `sEQ` [Ds_Eb,G,As_Bb             ] = Just KG_Ds_EbMajor -- Begin Ds_Eb
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
              | sp `sEQ` [Ds_Eb,F,G,As_Bb,Cs_Db     ] = Just KG_Ds_Eb9     -- End Ds_Eb
                                                        
              | sp `sEQ` [E,Gs_Ab,B                 ] = Just KG_EMajor     -- Begin E
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
              | sp `sEQ` [E,Fs_Gb,Gs_Ab,B,D         ] = Just KG_E9         -- End E
                                                        
              | sp `sEQ` [F,A,C                     ] = Just KG_F_EsMajor  -- Begin F_Es
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
              | sp `sEQ` [F,G,A,C,Ds_Eb             ] = Just KG_F_Es9      -- End F_Es
                                                        
              | sp `sEQ` [Fs_Gb,As_Bb,Cs_Db         ] = Just KG_Fs_GbMajor -- Begin Fs_Gb
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
              | sp `sEQ` [Fs_Gb,Gs_Ab,As_Bb,E       ] = Just KG_Fs_Gb9     --end Fs_Gb
                                                        
              | sp `sEQ` [G,B,D                     ] = Just KG_GMajor     --Begin G
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
              | sp `sEQ` [G,A,B,D,F                 ] = Just KG_Gs_Ab9     -- End G
                                                        
              | sp `sEQ` [Gs_Ab,C,Ds_Eb             ] = Just KG_Gs_AbMajor -- Begin Gs_Ab
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
              | sp `sEQ` [Gs_Ab,As_Bb,C,Ds_Eb,Fs_Gb ] = Just KG_Gs_Ab9     --end Gs_Ab
                                                        
              | sp `sEQ` [A,Cs_Db,E                 ] = Just KG_AMajor     --Begin A
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
              | sp `sEQ` [A,B,Cs_Db,E,G             ] = Just KG_A9         -- End A
                                                        
              | sp `sEQ` [As_Bb,D,F                 ] = Just KG_As_BbMajor -- Begin As_Bb
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
              | sp `sEQ` [As_Bb,C,D,F,Gs_Ab         ] = Just KG_As_Bb9     -- End As_Bb
                                                        
              | sp `sEQ` [B,Ds_Eb,Fs_Gb             ] = Just KG_B_CbMajor  -- Begin B_Cb
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
              | sp `sEQ` [B,Cs_Db,Ds_Eb,Fs_Gb,A     ] = Just KG_B_Cb9      -- End B_Cb
              | otherwise                             = Nothing            -- Broken [SigPitch] deserves Nothing !!!
  where
    sEQ :: (Eq a, Ord a) => [a] -> [a] -> Bool    -- Sort and compare two SigPitch lists
    sEQ a b = sort a == sort b

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


toMidi :: Note -> Int
toMidi A0      = 21
toMidi As_Bb_0 = 22
toMidi B0      = 23
toMidi C1      = 24
toMidi Cs_Db_1 = 25
toMidi D1      = 26
toMidi Ds_Eb_1 = 27
toMidi E1      = 28
toMidi F1      = 29
toMidi Fs_Gb_1 = 30
toMidi G1      = 31
toMidi Gs_Ab_1 = 32
toMidi A1      = 33
toMidi As_Bb_1 = 34
toMidi B1      = 35
toMidi C2      = 36
toMidi Cs_Db_2 = 37
toMidi D2      = 38
toMidi Ds_Eb_2 = 39
toMidi E2      = 40
toMidi F2      = 41
toMidi Fs_Gb_2 = 42
toMidi G2      = 43
toMidi Gs_Ab_2 = 44
toMidi A2      = 45
toMidi As_Bb_2 = 46
toMidi B2      = 47
toMidi C3      = 48
toMidi Cs_Db_3 = 49
toMidi D3      = 50
toMidi Ds_Eb_3 = 51
toMidi E3      = 52
toMidi F3      = 53
toMidi Fs_Gb_3 = 54
toMidi G3      = 55
toMidi Gs_Ab_3 = 56
toMidi A3      = 57
toMidi As_Bb_3 = 58
toMidi B3      = 59
toMidi C4      = 60
toMidi Cs_Db_4 = 61
toMidi D4      = 62
toMidi Ds_Eb_4 = 63
toMidi E4      = 64
toMidi F4      = 65
toMidi Fs_Gb_4 = 66
toMidi G4      = 67
toMidi Gs_Ab_4 = 68
toMidi A4      = 69
toMidi As_Bb_4 = 70
toMidi B4      = 71
toMidi C5      = 72
toMidi Cs_Db_5 = 73
toMidi D5      = 74
toMidi Ds_Eb_5 = 75
toMidi E5      = 76
toMidi F5      = 77
toMidi Fs_Gb_5 = 78
toMidi G5      = 79
toMidi Gs_Ab_5 = 80
toMidi A5      = 81
toMidi As_Bb_5 = 82
toMidi B5      = 83
toMidi C6      = 84
toMidi Cs_Db_6 = 85
toMidi D6      = 86
toMidi Ds_Eb_6 = 87
toMidi E6      = 88
toMidi F6      = 89
toMidi Fs_Gb_6 = 90
toMidi G6      = 91
toMidi Gs_Ab_6 = 92
toMidi A6      = 93
toMidi As_Bb_6 = 94
toMidi B6      = 95
toMidi C7      = 96
toMidi Cs_Db_7 = 97
toMidi D7      = 98
toMidi Ds_Eb_7 = 99
toMidi E7      = 100
toMidi F7      = 101
toMidi Fs_Gb_7 = 102
toMidi G7      = 103
toMidi Gs_Ab_7 = 104
toMidi A7      = 105
toMidi As_Bb_7 = 106
toMidi B7      = 107
toMidi C8      = 108

fromMidi :: Int -> Maybe Note
fromMidi 21  = Just A0
fromMidi 22  = Just As_Bb_0
fromMidi 23  = Just B0
fromMidi 24  = Just C1
fromMidi 25  = Just Cs_Db_1
fromMidi 26  = Just D1
fromMidi 27  = Just Ds_Eb_1
fromMidi 28  = Just E1
fromMidi 29  = Just F1
fromMidi 30  = Just Fs_Gb_1
fromMidi 31  = Just G1
fromMidi 32  = Just Gs_Ab_1
fromMidi 33  = Just A1
fromMidi 34  = Just As_Bb_1
fromMidi 35  = Just B1
fromMidi 36  = Just C2
fromMidi 37  = Just Cs_Db_2
fromMidi 38  = Just D2
fromMidi 39  = Just Ds_Eb_2
fromMidi 40  = Just E2
fromMidi 41  = Just F2
fromMidi 42  = Just Fs_Gb_2
fromMidi 43  = Just G2
fromMidi 44  = Just Gs_Ab_2
fromMidi 45  = Just A2
fromMidi 46  = Just As_Bb_2
fromMidi 47  = Just B2
fromMidi 48  = Just C3
fromMidi 49  = Just Cs_Db_3
fromMidi 50  = Just D3
fromMidi 51  = Just Ds_Eb_3
fromMidi 52  = Just E3
fromMidi 53  = Just F3
fromMidi 54  = Just Fs_Gb_3
fromMidi 55  = Just G3
fromMidi 56  = Just Gs_Ab_3
fromMidi 57  = Just A3
fromMidi 58  = Just As_Bb_3
fromMidi 59  = Just B3
fromMidi 60  = Just C4
fromMidi 61  = Just Cs_Db_4
fromMidi 62  = Just D4
fromMidi 63  = Just Ds_Eb_4
fromMidi 64  = Just E4
fromMidi 65  = Just F4
fromMidi 66  = Just Fs_Gb_4
fromMidi 67  = Just G4
fromMidi 68  = Just Gs_Ab_4
fromMidi 69  = Just A4
fromMidi 70  = Just As_Bb_4
fromMidi 71  = Just B4
fromMidi 72  = Just C5
fromMidi 73  = Just Cs_Db_5
fromMidi 74  = Just D5
fromMidi 75  = Just Ds_Eb_5
fromMidi 76  = Just E5
fromMidi 77  = Just F5
fromMidi 78  = Just Fs_Gb_5
fromMidi 79  = Just G5
fromMidi 80  = Just Gs_Ab_5
fromMidi 81  = Just A5
fromMidi 82  = Just As_Bb_5
fromMidi 83  = Just B5
fromMidi 84  = Just C6
fromMidi 85  = Just Cs_Db_6
fromMidi 86  = Just D6
fromMidi 87  = Just Ds_Eb_6
fromMidi 88  = Just E6
fromMidi 89  = Just F6
fromMidi 90  = Just Fs_Gb_6
fromMidi 91  = Just G6
fromMidi 92  = Just Gs_Ab_6
fromMidi 93  = Just A6
fromMidi 94  = Just As_Bb_6
fromMidi 95  = Just B6
fromMidi 96  = Just C7
fromMidi 97  = Just Cs_Db_7
fromMidi 98  = Just D7
fromMidi 99  = Just Ds_Eb_7
fromMidi 100 = Just E7
fromMidi 101 = Just F7
fromMidi 102 = Just Fs_Gb_7
fromMidi 103 = Just G7
fromMidi 104 = Just Gs_Ab_7
fromMidi 105 = Just A7
fromMidi 106 = Just As_Bb_7
fromMidi 107 = Just B7
fromMidi 108 = Just C8
fromMidi _   = Nothing

toNotesBySigPitch :: SigPitch -> [] Note
toNotesBySigPitch A     = A0      :A1      :A2      :A3      :A4      :A5      :A6      :A7          :[]
toNotesBySigPitch As_Bb = As_Bb_0 :As_Bb_1 :As_Bb_2 :As_Bb_3 :As_Bb_4 :As_Bb_5 :As_Bb_6 :As_Bb_7     :[]
toNotesBySigPitch B     = B0      :B1      :B2      :B3      :B4      :B5      :B6      :B7          :[]
toNotesBySigPitch C     =          C1      :C2      :C3      :C4      :C5      :C6      :C7      :C8 :[]
toNotesBySigPitch Cs_Db =          Cs_Db_1 :Cs_Db_2 :Cs_Db_3 :Cs_Db_4 :Cs_Db_5 :Cs_Db_6 :Cs_Db_7     :[]
toNotesBySigPitch D     =          D1      :D2      :D3      :D4      :D5      :D6      :D7          :[]
toNotesBySigPitch Ds_Eb =          Ds_Eb_1 :Ds_Eb_2 :Ds_Eb_3 :Ds_Eb_4 :Ds_Eb_5 :Ds_Eb_6 :Ds_Eb_7     :[]
toNotesBySigPitch E     =          E1      :E2      :E3      :E4      :E5      :E6      :E7          :[]
toNotesBySigPitch F     =          F1      :F2      :F3      :F4      :F5      :F6      :F7          :[]
toNotesBySigPitch Fs_Gb =          Fs_Gb_1 :Fs_Gb_2 :Fs_Gb_3 :Fs_Gb_4 :Fs_Gb_5 :Fs_Gb_6 :Fs_Gb_7     :[]
toNotesBySigPitch G     =          G1      :G2      :G3      :G4      :G5      :G6      :G7          :[]
toNotesBySigPitch Gs_Ab =          Gs_Ab_1 :Gs_Ab_2 :Gs_Ab_3 :Gs_Ab_4 :Gs_Ab_5 :Gs_Ab_6 :Gs_Ab_7     :[]

fromOctave :: Int -> [] Note
fromOctave 0 =                                                A0:As_Bb_0:B0 :[]
fromOctave 1 = C1:Cs_Db_1:D1:Ds_Eb_1:E1:F1:Fs_Gb_1:G1:Gs_Ab_1:A1:As_Bb_1:B1 :[]
fromOctave 2 = C2:Cs_Db_2:D2:Ds_Eb_2:E2:F2:Fs_Gb_2:G2:Gs_Ab_2:A2:As_Bb_2:B2 :[]
fromOctave 3 = C3:Cs_Db_3:D3:Ds_Eb_3:E3:F3:Fs_Gb_3:G3:Gs_Ab_3:A3:As_Bb_3:B3 :[]
fromOctave 4 = C4:Cs_Db_4:D4:Ds_Eb_4:E4:F4:Fs_Gb_4:G4:Gs_Ab_4:A4:As_Bb_4:B4 :[]
fromOctave 5 = C5:Cs_Db_5:D5:Ds_Eb_5:E5:F5:Fs_Gb_5:G5:Gs_Ab_5:A5:As_Bb_5:B5 :[]
fromOctave 6 = C6:Cs_Db_6:D6:Ds_Eb_6:E6:F6:Fs_Gb_6:G6:Gs_Ab_6:A6:As_Bb_6:B6 :[]
fromOctave 7 = C7:Cs_Db_7:D7:Ds_Eb_7:E7:F7:Fs_Gb_7:G7:Gs_Ab_7:A7:As_Bb_7:B7 :[]
fromOctave 8 = C8                                                           :[]
fromOctave _ = []

toNoteBySigOctave :: SigPitch -> Int -> Maybe Note
toNoteBySigOctave sp n = if null intersection
                          then Nothing
                          else Just (head intersection)
  where
    fo = fromOctave n
    tn = toNotesBySigPitch sp
    intersection = intersect fo tn

toOctave :: Note -> Int
toOctave A0      = 0
toOctave As_Bb_0 = 0
toOctave B0      = 0
toOctave C1      = 1
toOctave Cs_Db_1 = 1
toOctave D1      = 1
toOctave Ds_Eb_1 = 1
toOctave E1      = 1
toOctave F1      = 1
toOctave Fs_Gb_1 = 1
toOctave G1      = 1
toOctave Gs_Ab_1 = 1
toOctave A1      = 1
toOctave As_Bb_1 = 1
toOctave B1      = 1
toOctave C2      = 2
toOctave Cs_Db_2 = 2
toOctave D2      = 2
toOctave Ds_Eb_2 = 2
toOctave E2      = 2
toOctave F2      = 2
toOctave Fs_Gb_2 = 2
toOctave G2      = 2
toOctave Gs_Ab_2 = 2
toOctave A2      = 2
toOctave As_Bb_2 = 2
toOctave B2      = 2
toOctave C3      = 3
toOctave Cs_Db_3 = 3
toOctave D3      = 3
toOctave Ds_Eb_3 = 3
toOctave E3      = 3
toOctave F3      = 3
toOctave Fs_Gb_3 = 3
toOctave G3      = 3
toOctave Gs_Ab_3 = 3
toOctave A3      = 3
toOctave As_Bb_3 = 3
toOctave B3      = 3
toOctave C4      = 4
toOctave Cs_Db_4 = 4
toOctave D4      = 4
toOctave Ds_Eb_4 = 4
toOctave E4      = 4
toOctave F4      = 4
toOctave Fs_Gb_4 = 4
toOctave G4      = 4
toOctave Gs_Ab_4 = 4
toOctave A4      = 4
toOctave As_Bb_4 = 4
toOctave B4      = 4
toOctave C5      = 5
toOctave Cs_Db_5 = 5
toOctave D5      = 5
toOctave Ds_Eb_5 = 5
toOctave E5      = 5
toOctave F5      = 5
toOctave Fs_Gb_5 = 5
toOctave G5      = 5
toOctave Gs_Ab_5 = 5
toOctave A5      = 5
toOctave As_Bb_5 = 5
toOctave B5      = 5
toOctave C6      = 6
toOctave Cs_Db_6 = 6
toOctave D6      = 6
toOctave Ds_Eb_6 = 6
toOctave E6      = 6
toOctave F6      = 6
toOctave Fs_Gb_6 = 6
toOctave G6      = 6
toOctave Gs_Ab_6 = 6
toOctave A6      = 6
toOctave As_Bb_6 = 6
toOctave B6      = 6
toOctave C7      = 7
toOctave Cs_Db_7 = 7
toOctave D7      = 7
toOctave Ds_Eb_7 = 7
toOctave E7      = 7
toOctave F7      = 7
toOctave Fs_Gb_7 = 7
toOctave G7      = 7
toOctave Gs_Ab_7 = 7
toOctave A7      = 7
toOctave As_Bb_7 = 7
toOctave B7      = 7
toOctave C8      = 8

