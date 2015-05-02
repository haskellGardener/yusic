import Data.List (sort)


All Notes = [C,Cs_Db,D,Ds_Eb,E,F_Es,Fs_Gb,G,Gs_Ab,A,As_Bb,B]
Super All Notes = [A0,As0/Bb0,C1,Cs1/Db1,Ds1/Eb1,E1,F1,Fs1/Gb1,G1,Gs1/Ab1,A1,As1/Bb1,B1,C2,Cs2/Db2,Ds2/Eb2,E2,F2,Fs2/Gb2,G,Gs2/Ab2,A2,As2/Bb2,B2,C3,Cs3/Db3,Ds3/Eb3,E3,F3,Fs3/Gb3,G3,Gs3/Ab3,A3,As3/Bb3,B3,C4,Cs4/Db4,Ds4/Eb4,E4,F4,Fs4/Gb4,G4,Gs4/Ab4,A4,As4/Bb4,B4,C5,Cs5/Db5,Ds5/Eb5,E5,F5,Fs5/Gb5,G5,Gs5/Ab5,A5,As5/Bb5,B5,C6,Cs6/Db6,Ds6/Eb6,E6,F6,Fs6/Gb6,G6,Gs6/Ab6,A6,As6/Bb6,B6,C7,Cs7/Db7,Ds7/Eb7,E7,F7,Fs7/Gb7,G7,Gs7/Ab7,A7,As7/Bb7,B7,C8]
Super Midi Notes = [21,22,23,24,25,26,27,28,29,30,31,32,33,34,35,36,37,38,39,40,41,42,43,44,45,46,47,48,49,50,51,52,53,54,55,56,57,58,59,60,61,62,63,64,65,66,67,68,69,70,71,72,73,74,75,76,77,78,79,80,81,82,83,84,85,86,87,88,89,90,91,92,93,94,95,96,97,98,99,100,101,102,103,104,105,106,107,108]


data SigPitch = C
              | D
              | Ds_Ab
              | E
              | F
              | G
              | A
              | As_Bb
              | B
                deriving (Show, Ord, Eq) -- You can now perform math ops on SigPitch (e.g. (As < B), (C /= D), (G == G), etc.)
                                         -- Which means you can  sort [E, G, C] = [C,E,G] (same as C:E:G:[]) inside toCanonicalSignature
                                         -- and order is correct.


data CanonicalSignature = CMajor  --Begin C
                        | CMinor
                        | C5
                        | CDominant7
                        | CMajor7
                        | CMinor7
                        | CMinorMajor7
                        | CSus4
                        | CSus2
                        | C6
                        | CMinor
                        | C9    - End C
                        | Cs_Db  - Begin Cs/Db
                        | Cs_DbMinor
                        | Cs_Db5
                        | Cs_DbDominant7
                        | Cs_DbMajor7
                        | Cs_DbMinor7
                        | Cs_DbMinorMajor7
                        | Cs_DbSus4
                        | Cs_DbSus2
                        | Cs_Db6
                        | Cs_DbMinor
                        | Cs_Db9  -End Cs/Db
                        | DMajor  --Begin D
                        | DMinor
                        | D5
                        | DDominant7
                        | DMajor7
                        | DMinor7
                        | DMinorMajor7
                        | DSus4
                        | DSus2
                        | D6
                        | DMinor
                        | D9   -- End D 
                        | Ds_EbMajor  --Begin Ds/Eb
                        | Ds_EbMinor
                        | Ds_Eb5
                        | Ds_EbDominant7
                        | Ds_EbMajor7
                        | Ds_EbMinor7
                        | Ds_EbMinorMajor7
                        | Ds_EbSus4
                        | Ds_EbSus2
                        | Ds_Eb6
                        | Ds_EbMinor
                        | Ds_Eb9   -- End Ds/Eb
                        | EMajor  --Begin E
                        | EMinor
                        | E5
                        | EDominant7
                        | EMajor7
                        | EMinor7
                        | EMinorMajor7
                        | ESus4
                        | ESus2
                        | E6
                        | EMinor
                        | E9   -- End E
                        | F_EsMajor  --Begin F/Es
                        | F_EsMinor
                        | F_Es5
                        | F_EsDominant7
                        | F_EsMajor7
                        | F_EsMinor7
                        | F_EsMinorMajor7
                        | F_EsSus4
                        | F_EsSus2
                        | F_Es6
                        | F_EsMinor
                        | F_Es9   -- End F/Es
                        | Fs_EbMajor  --Begin Fs/Eb
                        | Fs_EbMinor
                        | Fs_Eb5
                        | Fs_EbDominant7
                        | Fs_EbMajor7
                        | Fs_EbMinor7
                        | Fs_EbMinorMajor7
                        | Fs_EbSus4
                        | Fs_EbSus2
                        | Fs_Eb6
                        | Fs_EbMinor
                        | Fs_Eb9   -- End Fs/Eb
                        | GMajor  --Begin G
                        | GMinor
                        | G5
                        | GDominant7
                        | GMajor7
                        | GMinor7
                        | GMinorMajor7
                        | GSus4
                        | GSus2
                        | G6
                        | GMinor
                        | G9   -- End G
                        | Gs_AbMajor  --Begin Gs/Ab
                        | Gs_AbMinor
                        | Gs_Ab5
                        | Gs_AbDominant7
                        | Gs_AbMajor7
                        | Gs_AbMinor7
                        | Gs_AbMinorMajor7
                        | Gs_AbSus4
                        | Gs_AbSus2
                        | Gs_Ab6
                        | Gs_AbMinor
                        | Gs_Ab9   -- End Gs/Ab
                        | AMajor  --Begin A
                        | AMinor
                        | A5
                        | ADominant7
                        | AMajor7
                        | AMinor7
                        | AMinorMajor7
                        | ASus4
                        | ASus2
                        | A6
                        | AMinor
                        | A9   -- End A
                        | As_BbMajor  --Begin As/Bb
                        | As_BbMinor
                        | As_Bb5
                        | As_BbDominant7
                        | As_BbMajor7
                        | As_BbMinor7
                        | As_BbMinorMajor7
                        | As_BbSus4
                        | As_BbSus2
                        | As_Bb6
                        | As_BbMinor
                        | As_Bb9   -- End As/Bb
                        | B_CbMajor  --Begin B/Cb
                        | B_CbMinor
                        | B_Cb5
                        | B_CbDominant7
                        | B_CbMajor7
                        | B_CbMinor7
                        | B_CbMinorMajor7
                        | B_CbSus4
                        | B_CbSus2
                        | B_Cb6
                        | B_CbMinor
                        | B_Cb9   -- End B/Cb
                        

toCanonicalSignature :: [SigPitch] -> Maybe CanonicalSignature       -- Account for broken [SigPitch]
toCanonicalSignature [] = Nothing
toCanonicalSignature sp | sp `sEQ` [C,E,G     				] = Just CMajor         -- sort both expected and positional parameter and compare
                        | sp `sEQ` [C,Ds_Eb,G 				] = Just CMinor
                        | sp `sEQ` [C,G       				] = Just C5
                        | sp `sEQ` [C,E,G,As_Bb 			] = Just CDominant7
                        | sp `sEQ` [C,E,G,B   				] = Just CMajor7
                        | sp `sEQ` [C,Ds_Eb,G,As			] = Just CMinor7
                        | sp `sEQ` [C,Ds_Eb,G,B 			] = Just CMinorMajor7
                        | sp `sEQ` [C,F,G     				] = Just CSus4
                        | sp `sEQ` [C,D,G     				] = Just CSus2
                        | sp `sEQ` [C,E,G,A   				] = Just C6
                        | sp `sEQ` [C,Ds_Eb,G,A 			] = Just CMinor6
                        | sp `sEQ` [C,E,G,As  				] = Just C9
                        | otherwise            	 			  = Nothing             -- Broken [SigPitch] deserves Nothing !!!
                        | sp `sEQ` [Cs_Db,F,Gs_Ab			] = Just Cs_DbMajor			-- Begin Cs_DbMajor
                        | sp `sEQ` [Cs_Db,E,Gs_Ab			] = Just Cs_DbMinor
                        | sp `sEQ` [Cs_Db,Gs_Ab				]   = Just Cs_Db5
                        | sp `sEQ` [Cs_Db,G,Gs_Ab,B			] = Just Cs_DbDominant7
                        | sp `sEQ` [Cs_Db,F,Gs_Ab,C			] = Just Cs_DbMajor7
                        | sp `sEQ` [Cs_Db,E,Gs_Ab,B			] = Just Cs_DbMinor7
                        | sp `sEQ` [Cs_Db,E,Gs_Ab,C			] = Just Cs_DbMinorMajor7
                        | sp `sEQ` [Cs_Db,Fs_Gb,Gs_Ab			] = Just Cs_DbSus4
                        | sp `sEQ` [Cs_Db,Ds_Eb,Gs_Ab			] = Just Cs_DbSus2
                        | sp `sEQ` [Cs_Db,F,Gs_Ab,As_Ab			] = Just Cs_Db6
                        | sp `sEQ` [Cs_Db,E,Gs_Ab,As_Bb			] = Just Cs_DbMinor6
                        | sp `sEQ` [Cs_Db,Ds_Eb,F,Gs_Ab,B		] = Just Cs_Db9    			--end Cs_Db
                        | sp `sEQ` [D,Fs_Ab,A				] = Just DMajor   		 	--Begin D
                        | sp `sEQ` [D,F,A					] = Just DMinor   
                        | sp `sEQ` [D,A						] = Just D5
                        | sp `sEQ` [D,Fs_Gb,A,C				] = Just DDominant7
                        | sp `sEQ` [D,Fs_Gb,A,Cs_Db			] = Just DMajor7
                        | sp `sEQ` [D,F,A,C					] = Just DMinor7
                        | sp `sEQ` [D,F,G,Cs_Db				] = Just DMinorMajor7
                        | sp `sEQ` [D,G,A					] = Just DSus4
                        | sp `sEQ` [D,E,A					] = Just DSus2
                        | sp `sEQ` [D,Fs_Gb,A,B				] = Just D6
                        | sp `sEQ` [D,F,A,B					] = Just DMinor6
                        | sp `sEQ` [D,E,Fs_Gb,A,C			] = Just D9					--end DMajor     
                        | sp `sEQ` [Ds_Eb,G,As_Bb			] = Just Ds_EbMajor         --Begin Ds_Eb
                        | sp `sEQ` [Ds_Eb,Fs_Gb,As_Bb			] = Just Ds_EbMinor   
                        | sp `sEQ` [Ds_Eb,As_Bb				] = Just Ds_Eb5
                        | sp `sEQ` [Ds_Eb,G,As_Bb,Cs_Db			] = Just Ds_EbDominant7
                        | sp `sEQ` [Ds_Eb,G,As_Bb,D			] = Just Ds_EbMajor7
                        | sp `sEQ` [Ds_Eb,Fs_Gb,As_Bb,Cs_Db		] = Just Ds_EbMinor7
                        | sp `sEQ` [Ds_Eb,Fs_Gb,As_Bb,D			] = Just Ds_EbMinorMajor7
                        | sp `sEQ` [Ds_Eb,Gs_Ab,As_Bb			] = Just Ds_EbSus4
                        | sp `sEQ` [Ds_Eb,F,As_Bb			] = Just Ds_EbSus2
                        | sp `sEQ` [Ds_Eb,G,As_Bb,C			] = Just Ds_Eb6
                        | sp `sEQ` [Ds_Eb,Fs_Gb,As_Bb,C			] = Just Ds_EbMinor6
                        | sp `sEQ` [Ds_Eb,F,G,As_Bb,Cs_Db		] = Just Ds_Eb9				--end Ds_Eb     
                        | sp `sEQ` [E,Gs_Ab,B				] = Just EMajor             --Begin E
                        | sp `sEQ` [E,G,B					] = Just EMinor   
                        | sp `sEQ` [E,B						] = Just E5
                        | sp `sEQ` [E,Gs_Ab,B,D				] = Just EDominant7
                        | sp `sEQ` [E,Gs_Ab,B,Ds_Eb			] = Just EMajor7
                        | sp `sEQ` [E,G,B,D					] = Just EMinor7
                        | sp `sEQ` [E,G,B,Cs_Eb				] = Just EMinorMajor7
                        | sp `sEQ` [E,A,B					] = Just ESus4
                        | sp `sEQ` [E,Fs_Gb,B				] = Just ESus2
                        | sp `sEQ` [E,Gs_Ab,B,Cs_Db			] = Just E6
                        | sp `sEQ` [E,G,B,Cs_Db				] = Just EMinor6
                        | sp `sEQ` [E,Fs_Gb,Gs_Ab,B,D			] = Just E9					--end E        
                        | sp `sEQ` [F_Es,A,C				] = Just F_EsMajor          --Begin F_Es
                        | sp `sEQ` [F_Es,Gs_Ab,C			] = Just F_EsMinor   
                        | sp `sEQ` [F_Es,C					] = Just F_Es5
                        | sp `sEQ` [F_Es,G,C,Ds_Eb			] = Just F_EsDominant7
                        | sp `sEQ` [F_Es,A,C,E				] = Just F_EsMajor7
                        | sp `sEQ` [F_Es,Gs_Ab,C,Ds_Eb			] = Just F_EsMinor7
                        | sp `sEQ` [F_Es,Gs_Ab,C,E			] = Just F_EsMinorMajor7
                        | sp `sEQ` [F_Es,As_Bb,C			] = Just F_EsSus4
                        | sp `sEQ` [F_Es,G,C				] = Just F_EsSus2
                        | sp `sEQ` [F_Es,A,C,D				] = Just F_Es6
                        | sp `sEQ` [F_Es,Gs_Ab,C,D			] = Just F_EsMinor6
                        | sp `sEQ` [F_Es,G,A,C,Ds_Eb			] = Just F_Es9				--end F_Es
                        | sp `sEQ` [Fs_Gb,As_Bb,Ds_Db			] = Just Fs_GbMajor         --Begin Fs_Gb
                        | sp `sEQ` [Fs_Gb,A,Cs_Db			] = Just Fs_GbMinor   
                        | sp `sEQ` [Fs_Gb,Cs_Db				] = Just Fs_Gb5
                        | sp `sEQ` [Fs_Gb,Fs_Gb,As_Bb,E			] = Just Fs_GbDominant7
                        | sp `sEQ` [Fs_Gb,As_Bb,F			] = Just Fs_GbMajor7
                        | sp `sEQ` [Fs_Gb,A,Cs_Db,E			] = Just Fs_GbMinor7
                        | sp `sEQ` [Fs_Gb,A,Cs_Db,F			] = Just Fs_GbMinorMajor7
                        | sp `sEQ` [Fs_Gb,B,Cs_Db			] = Just Fs_GbSus4
                        | sp `sEQ` [Fs_Gb,Gs_Ab,Cs_Db			] = Just Fs_GbSus2
                        | sp `sEQ` [Fs_Gb,As_Bb,Cs_Db,Ds_Eb		] = Just Fs_Gb6
                        | sp `sEQ` [Fs_Gb,A,Cs_Db,Ds_Eb			] = Just Fs_GbMinor6
                        | sp `sEQ` [Fs_Gb,Gs_Ab,As_Bb,E			] = Just Fs_Gb9				--end Fs_Gb
                        | sp `sEQ` [G,B,D					] = Just GMajor             --Begin G
                        | sp `sEQ` [G,As_Bb,D				] = Just GMinor   
                        | sp `sEQ` [G,D						] = Just G5
                        | sp `sEQ` [G,B,D,F					] = Just GDominant7
                        | sp `sEQ` [G,B,D,Fs_Gb				] = Just GMajor7
                        | sp `sEQ` [G,As_Bb,D,F				] = Just GMinor7
                        | sp `sEQ` [G,As_Bb,D,Fs_Gb			] = Just GMinorMajor7
                        | sp `sEQ` [G,C,D					] = Just GSus4
                        | sp `sEQ` [G,A,D					] = Just GSus2
                        | sp `sEQ` [G,B,D,E					] = Just G6
                        | sp `sEQ` [G,As_Bb,D,E				] = Just Gs_AbMinor6
                        | sp `sEQ` [G,A,B,D,F				] = Just Gs_Ab9				--end G
                        | sp `sEQ` [Gs_Ab,C,Ds_Eb			] = Just Gs_AbMajor         --Begin Gs_Ab
                        | sp `sEQ` [Gs_Ab,B,Ds_Eb			] = Just Gs_AbMinor   
                        | sp `sEQ` [Gs_Ab,Ds_Eb				] = Just Gs_Ab5
                        | sp `sEQ` [Gs_Ab,C,Ds_Eb,Cs_Db			] = Just Gs_AbDominant7
                        | sp `sEQ` [Gs_Ab,C,Ds_Eb,G			] = Just Gs_AbMajor7
                        | sp `sEQ` [Gs_Ab,B,Ds_Bb,Cs_Db			] = Just Gs_AbMinor7
                        | sp `sEQ` [Gs_Ab,B,Ds_Eb,G			] = Just Gs_AbMinorMajor7
                        | sp `sEQ` [Gs_Ab,Cs_Db,Ds_Eb			] = Just Gs_AbSus4
                        | sp `sEQ` [Gs_Ab,As_Bb,Ds_Eb			] = Just Gs_AbSus2
                        | sp `sEQ` [Gs_Ab,C,Ds_Eb,F			] = Just Gs_Ab6
                        | sp `sEQ` [Gs_Ab,B,Ds_Eb,F			] = Just Gs_AbMinor6
                        | sp `sEQ` [Gs_Ab,As_Bb,C,Ds_Eb,Fs_Gb] = Just Gs_Ab9			--end Gs_Ab
                        | sp `sEQ` [A,Cs_Db,E				] = Just AMajor             --Begin A
                        | sp `sEQ` [A,C,E				] = Just AMinor   
                        | sp `sEQ` [A,E					] = Just A5
                        | sp `sEQ` [A,Cs_Db,E,G				] = Just ADominant7
                        | sp `sEQ` [A,Cs_Db,E,Gs_Ab			] = Just AMajor7
                        | sp `sEQ` [A,C,E,G				] = Just AMinor7
                        | sp `sEQ` [A,C,E,Gs_Ab				] = Just AMinorMajor7
                        | sp `sEQ` [A,D,E				] = Just ASus4
                        | sp `sEQ` [A,B,E				] = Just ASus2
                        | sp `sEQ` [A,Cs_Db,Fs_Gb			] = Just A6
                        | sp `sEQ` [A,C,E,Fs_Gb				] = Just AMinor6
                        | sp `sEQ` [A,B,Cs_Db,E,G			] = Just A9					--end A
                        | sp `sEQ` [As_Bb,D,F				] = Just As_BbMajor         --Begin As_Bb
                        | sp `sEQ` [As_Bb,Cs_Db,F			] = Just As_BbMinor   
                        | sp `sEQ` [As_Bb,F				] = Just As_Bb5
                        | sp `sEQ` [As_Bb,D,F,Gs_Ab			] = Just As_BbDominant7
                        | sp `sEQ` [As_Bb,D,F,A				] = Just As_BbMajor7
                        | sp `sEQ` [As_Bb,Cs_Db,F,Gs_Ab			] = Just As_BbMinor7
                        | sp `sEQ` [As_Bb,Cs_Db,F,A			] = Just As_BbMinorMajor7
                        | sp `sEQ` [As_Bb,Ds_Eb,F			] = Just As_BbSus4
                        | sp `sEQ` [As_Bb,C,F				] = Just As_BbSus2
                        | sp `sEQ` [As_Bb,D,F,G				] = Just As_Bb6
                        | sp `sEQ` [As_Bb,Cs_Db,F,G			] = Just As_BbMinor6
                        | sp `sEQ` [As_Bb,C,D,F,Gs_Ab			] = Just As_Bb9				--end As_Bb
                        | sp `sEQ` [B,Ds_Eb,Fs_Gb			] = Just B_CbMajor          --Begin B_Cb
                        | sp `sEQ` [B,D,Fs_Gb				] = Just B_CbMinor   
                        | sp `sEQ` [B,Fs_Gb				] = Just B_Cb5
                        | sp `sEQ` [B,Ds_Eb,Fs_Gb,A			] = Just B_CbDominant7
                        | sp `sEQ` [B,Ds_Eb,Fs_Gb,As_Bb			] = Just B_CbMajor7
                        | sp `sEQ` [B,D,Fs_Gb,A				] = Just B_CbMinor7
                        | sp `sEQ` [B,D,Fs_Gb,As_Bb			] = Just B_CbMinorMajor7
                        | sp `sEQ` [B,E,Fs_Gb				] = Just B_CbSus4
                        | sp `sEQ` [B,Cs_Db,Fs_Gb			] = Just B_CbSus2
                        | sp `sEQ` [B,Ds_Eb,Fs_Gb,GS_Ab			] = Just B_Cb6
                        | sp `sEQ` [B,D,Fs_Gb,Gs_Ab			] = Just B_CbMinor6
                        | sp `sEQ` [B,Cs_Db,Ds_Eb,Fs_Gb,A		] = Just B_Cb9				--end B_Cb
  where
    sEQ :: [SigPitch] -> [SigPitch] -> Bool
    sEQ a b = sort a == sort b




fromCanonicalSignature :: CanonicalSignature -> [SigPitch]
fromCanonicalSignature CMajor = C:E:G:[]     -- Same as [C,E,G]							—Begin C
fromCanonicalSignature CMinor = C:Eb:G:[]    -- Same as [C,Eb,G]
fromCanonicalSignature C5 = C:G:[]
fromCanonicalSignature CDominant7 = C:E:G:As:[]
fromCanonicalSignature CMajor7 = C:E:G:B:[]
fromCanonicalSignature CMinor7 = C:Eb:G:As:[]
fromCanonicalSignature CMinorMajor7 = C:Eb:G:B:[]
fromCanonicalSignature CSus4 = C:F:G:[]
fromCanonicalSignature CSus2 = C:D:G:[]
fromCanonicalSignature C6 = C:E:G:A:[]
fromCanonicalSignature CMinor6 = C:Eb:G:A:[]
fromCanonicalSignature C9 = C:E:G:As:[]										—End C
														--BeginCs/Db

fromCanonicalSignature Cs_DbMajor = Cs_Db:F:Gs_Ab:[]     -- 
fromCanonicalSignature Cs_DbMinor = Cs_Db:E:Gs_Ab:[]  -- 
fromCanonicalSignature Cs_Db5 =  Cs_Db:Gs_Ab:[]
fromCanonicalSignature Cs_DbDominant7 = Cs_Db:F:Gs_Ab:B:[]
fromCanonicalSignature Cs_DbMajor7 = Cs_Db:F:Gs_Ab:C:[]
fromCanonicalSignature Cs_DbMinor7 = Cs_Db:E:Gs_Ab:B:[]
fromCanonicalSignature Cs_DbMinorMajor7 = Cs_Db:E:Gs_Ab:C:[]
fromCanonicalSignature Cs_DbSus4 = Cs_Db:Fs_Gb:Gs:Ab:[]
fromCanonicalSignature Cs_DbSus2 = Cs_Db:Ds_Eb:Gs_Ab:[]
fromCanonicalSignature Cs_Db6 = Cs_Db:F:Gs_Ab:As_Bb:[]
fromCanonicalSignature Cs_DbMinor6 = Cs_Db:E:Gs_Ab:As_Bb:[]
fromCanonicalSignature Cs_Db9 = Cs_Db:Ds_Eb:F:Gs_Ab:B:[]

fromCanonicalSignature DMajor = D:Fs_Ab:A:[]     								—Begin D
fromCanonicalSignature DMinor = D:F:A:[]  -- 
fromCanonicalSignature D5 =  D:A:[]
fromCanonicalSignature DDominant7 = D:Fs_Gb:A:C	:[]
fromCanonicalSignature DMajor7 = D:Fs_Gb:A:Cs_Db:[]
fromCanonicalSignature DMinor7 = D:F:A:C:[]
fromCanonicalSignature DMinorMajor7 = D:F:G:Cs_Db:[]
fromCanonicalSignature DSus4 = D:G:A:[]
fromCanonicalSignature DSus2 = D:E:A:[]
fromCanonicalSignature D6 = D:Fs_Gb:A:B:[]
fromCanonicalSignature DMinor6 = D:F:A:B:[]
fromCanonicalSignature D9 = D:E:Fs_Gb:A:C:[]									—End D

fromCanonicalSignature Ds_EbMajor = Ds_Eb:G:As_Bb:[]     								—Begin Ds_Eb
fromCanonicalSignature Ds_EbMinor = Ds_Eb:Fs_Gb:As_Bb:[]  -- 
fromCanonicalSignature Ds_Eb5 =  Ds_Eb:As_Bb:[]
fromCanonicalSignature Ds_EbDominant7 = Ds_Eb:G:As_Bb:Cs_Db:[]
fromCanonicalSignature Ds_EbMajor7 = Ds_Eb:G:As_Bb:D:[]
fromCanonicalSignature Ds_EbMinor7 = Ds_Eb:Fs_Gb:As_Bb:Cs_Db:[]
fromCanonicalSignature Ds_EbMinorMajor7 = Ds_Eb:Fs_Gb:As_Bb:D:[]
fromCanonicalSignature Ds_EbSus4 = Ds_Eb:Gs_Ab:As_Bb:[]
fromCanonicalSignature Ds_EbSus2 = Ds_Eb:F:As_Bb:[]
fromCanonicalSignature Ds_Eb6 = Ds_Eb:G:As_Bb:C:[]
fromCanonicalSignature Ds_EbMinor6 = Ds_Eb:Fs_Gb:As_Bb:C:[]
fromCanonicalSignature Ds_Eb9 = Ds_Eb:F:G:As_Bb:Cs_Db:[]									—End Ds_Eb

fromCanonicalSignature EMajor = E:Gs_Ab:B:[]     								—Begin E
fromCanonicalSignature EMinor = E:G:B:[]  -- 
fromCanonicalSignature E5 =  E:B:[]
fromCanonicalSignature EDominant7 = E:Gs_Ab:B:D:[]
fromCanonicalSignature EMajor7 = E:Gs_Ab:B:Ds_Eb:[]
fromCanonicalSignature EMinor7 = E:G:B:D:[]
fromCanonicalSignature EMinorMajor7 = E:G:B:Cs_Eb:[]
fromCanonicalSignature ESus4 = E:A:B:[]
fromCanonicalSignature ESus2 = E:Fs_Gb:B:[]
fromCanonicalSignature E6 = E:Gs_Ab:B:Cs_Db:[]
fromCanonicalSignature EMinor6 = E:G:B:Cs_Db:[]
fromCanonicalSignature E9 = E:Fs_Gb:Gs_Ab:B:D:[]									—End E


fromCanonicalSignature F_EsMajor = F_Es:A:C:[]     								—Begin F_Es
fromCanonicalSignature F_EsMinor = F_Es:Gs_Ab:C:[]  -- 
fromCanonicalSignature F_Es5 =  F_Es:C:[]
fromCanonicalSignature F_EsDominant7 = F_Es:G:C:Ds_Eb:[]
fromCanonicalSignature F_EsMajor7 = F_Es:A:C:E:[]
fromCanonicalSignature F_EsMinor7 = F_Es:Gs_Ab:C:Ds_Eb:[]
fromCanonicalSignature F_EsMinorMajor7 = F_Es:Gs_Ab:C:E:[]
fromCanonicalSignature F_EsSus4 = F_Es:As_Bb:C:[]
fromCanonicalSignature F_EsSus2 = F_Es:G:C:[]
fromCanonicalSignature F_Es6 = F_Es:A:C:D:[]
fromCanonicalSignature F_EsMinor6 = F_Es:Gs_Ab:C:D:[]
fromCanonicalSignature F_Es9 = F_Es:G:A:C:Ds_Eb:[]									—End F_Es

fromCanonicalSignature Fs_GbMajor = Fs_Gb:As_Bb:Ds_Db:[]     								—Begin Fs_Gb
fromCanonicalSignature Fs_GbMinor = Fs_Gb:A:Cs_Db:[]  -- 
fromCanonicalSignature Fs_Gb5 =  Fs_Gb:Cs_Db:[]
fromCanonicalSignature Fs_GbDominant7 = Fs_Gb:Fs_Gb:As_Bb:E:[]
fromCanonicalSignature Fs_GbMajor7 = Fs_Gb:As_Bb:F:[]
fromCanonicalSignature Fs_GbMinor7 = Fs_Gb:A:Cs_Db:E:[]
fromCanonicalSignature Fs_GbMinorMajor7 = Fs_Gb:A:Cs_Db:F:[]
fromCanonicalSignature Fs_GbSus4 = Fs_Gb:B:Cs_Db:[]
fromCanonicalSignature Fs_GbSus2 = Fs_Gb:Gs_Ab:Cs_Db:[]
fromCanonicalSignature Fs_Gb6 = Fs_Gb:As_Bb:Cs_Db:Ds_Eb:[]
fromCanonicalSignature Fs_GbMinor6 = Fs_Gb:A:Cs_Db:Ds_Eb:[]
fromCanonicalSignature Fs_Gb9 = Fs_Gb:Gs_Ab:As_Bb:E:[]									—End Fs_Gb

fromCanonicalSignature GMajor = G,B,D:[]     								—Begin G
fromCanonicalSignature GMinor = G,As_Bb,D:[]  -- 
fromCanonicalSignature G5 =  G,D:[]
fromCanonicalSignature GDominant7 = G,B,D,F:[]
fromCanonicalSignature GMajor7 = G,B,D,Fs_Gb:[]
fromCanonicalSignature GMinor7 = G,As_Bb,D,F:[]
fromCanonicalSignature GMinorMajor7 = G,As_Bb,D,Fs_Gb:[]
fromCanonicalSignature GSus4 = G,C,D:[]
fromCanonicalSignature GSus2 = G,A,D:[]
fromCanonicalSignature G6 = G,B,D,E:[]
fromCanonicalSignature GMinor6 = G:As_Bb:D:E:[]
fromCanonicalSignature G9 = G:A:B:D:F:[]									—End G

fromCanonicalSignature Gs_AbMajor = Gs_Ab:C:Ds_Eb:[]     								—Begin Gs_Ab
fromCanonicalSignature Gs_AbMinor = Gs_Ab:B:Ds_Eb:[]  -- 
fromCanonicalSignature Gs_Ab5 =  Gs_Ab:Ds_Eb:[]
fromCanonicalSignature Gs_AbDominant7 = Gs_Ab:C:Ds_Eb:Cs_Db:[]
fromCanonicalSignature Gs_AbMajor7 = Gs_Ab:C:Ds_Eb:G:[]
fromCanonicalSignature Gs_AbMinor7 = Gs_Ab:B:Ds_Bb:Cs_Db:[]
fromCanonicalSignature Gs_AbMinorMajor7 = Gs_Ab:B:Ds_Eb:G:[]
fromCanonicalSignature Gs_AbSus4 = Gs_Ab:Cs_Db:Ds_Eb:[]
fromCanonicalSignature Gs_AbSus2 = Gs_Ab:As_Bb:Ds_Eb:[]
fromCanonicalSignature Gs_Ab6 = Gs_Ab:C:Ds_Eb:F	:[]
fromCanonicalSignature Gs_AbMinor6 = Gs_Ab:B:Ds_Eb:F:[]
fromCanonicalSignature Gs_Ab9 = Gs_Ab:As_Bb:C:Ds_Eb:Fs_Gb:[]									—End Gs_Ab

fromCanonicalSignature AMajor = A:Cs_Db:E:[]     								—Begin A
fromCanonicalSignature AMinor = A:C:E:[]  -- 
fromCanonicalSignature A5 =  A:E:[]
fromCanonicalSignature ADominant7 = A:Cs_Db:E:G:[]
fromCanonicalSignature AMajor7 = A:Cs_Db:E:Gs_Ab:[]
fromCanonicalSignature AMinor7 = A:C:E:G:[]
fromCanonicalSignature AMinorMajor7 = A:C:E:Gs_Ab:[]
fromCanonicalSignature ASus4 = A:D:E:[]
fromCanonicalSignature ASus2 = A:B:E:[]
fromCanonicalSignature A6 = A:Cs_Db:Fs_Gb:[]
fromCanonicalSignature AMinor6 = A:C:E:Fs_Gb:[]
fromCanonicalSignature A9 = A:B:Cs_Db:E:G:[]									—End A

fromCanonicalSignature As_BbMajor = As_Bb:D:F:[]     								—Begin As_Bb
fromCanonicalSignature As_BbMinor = As_Bb:Cs_Db:F:[]  -- 
fromCanonicalSignature As_Bb5 = As_Bb:F:[]
fromCanonicalSignature As_BbDominant7 = As_Bb:D:F,Gs_Ab:[]
fromCanonicalSignature As_BbMajor7 = As_Bb:D:F:A:[]
fromCanonicalSignature As_BbMinor7 = As_Bb:Cs_Db:F:Gs_Ab:[]
fromCanonicalSignature As_BbMinorMajor7 = As_Bb:Cs_Db:F:A:[]
fromCanonicalSignature As_BbSus4 = As_Bb:Ds_Eb:F:[]
fromCanonicalSignature As_BbSus2 = As_Bb:C:F:[]
fromCanonicalSignature As_Bb6 = As_Bb:D:F:G:[]
fromCanonicalSignature As_BbMinor6 = As_Bb:Cs_Db:F:G:[]
fromCanonicalSignature As_Bb9 = As_Bb:C:D:F:Gs_Ab:[]									—End As_Bb

fromCanonicalSignature B_CbMajor = B:Ds_Eb:Fs_Gb:[]     								—Begin B_Cb
fromCanonicalSignature B_CbMinor = B:D:Fs_Gb:[]  -- 
fromCanonicalSignature B_Cb5 = B:Fs_Gb:[]
fromCanonicalSignature B_CbDominant7 = B:Ds_Eb:Fs_Gb:A:[]
fromCanonicalSignature B_CbMajor7 = B:Ds_Eb:Fs_Gb:As_Bb:[]
fromCanonicalSignature B_CbMinor7 = B:D:Fs_Gb:A:[]
fromCanonicalSignature B_CbMinorMajor7 = B:D:Fs_Gb:As_Bb:[]
fromCanonicalSignature B_CbSus4 = B:E:Fs_Gb:[]
fromCanonicalSignature B_CbSus2 = B:Cs_Db:Fs_Gb:[]
fromCanonicalSignature B_Cb6 = B:Ds_Eb:Fs_Gb:GS_Ab:[]
fromCanonicalSignature B_CbMinor6 = B:D:Fs_Gb:Gs_Ab:[]
fromCanonicalSignature B_Cb9 = B:Cs_Db:Ds_Eb:Fs_Gb:A:[]									—End B_Cb


