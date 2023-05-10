-module(dart_format).
-compile(export_all).

start() ->
    Rects =
        [{box,{399,145,46,8},[226,128,148]},
         {box,{558,146,16,2},[226,128,148]},
         {box,{342,181,41,17},"SEF"},
         {box,{412,178,30,27},"O"},
         {box,{498,181,146,20},"Departamento"},
         {box,{652,183,5,12},":"},
         {box,{95,258,77,13},"APELIDO"},
         {box,{95,277,70,14},"Surname"},
         {box,{171,278,4,13},"#"},
         {box,{181,278,36,13},"Nom"},
         {box,{95,306,63,14},"NOMES"},
         {box,{164,304,91,16},"PROPRIOS"},
         {box,{95,325,35,13},"Give"},
         {box,{136,329,51,10},"names"},
         {box,{192,326,5,13},"/"},
         {box,{203,326,60,13},"PrÃ©nom"},
         {box,{94,355,146,13},"NACIONALIDADE"},
         {box,{94,376,85,16},"Nactionaliy"},
         {box,{184,376,5,12},"/"},
         {box,{194,376,81,13},"Nationalita"},
         {box,{789,406,13,13},"M"},
         {box,{828,402,27,25},"oO"},
         {box,{94,402,47,13},"DATA"},
         {box,{147,403,23,12},"DE"},
         {box,{176,403,117,13},"NASCIMENTO"},
         {box,{675,407,48,13},"SEXO"},
         {box,{93,423,35,13},"Birth"},
         {box,{134,424,33,12},"date"},
         {box,{172,423,5,13},"/"},
         {box,{183,424,21,12},"NÃ©"},
         {box,{210,425,12,12},"le"},
         {box,{675,428,29,13},"Sex"},
         {box,{709,428,6,13},"/"},
         {box,{720,428,40,13},"Sexe"},
         {box,{92,451,120,13},"PASSAPORTE"},
         {box,{317,453,74,13},"BILHETE"},
         {box,{397,454,23,13},"DE"},
         {box,{426,454,106,15},"IDENTIDADE"},
         {box,{580,453,26,25},"(0"},
         {box,{631,457,61,13},"outro"},
         {box,{92,471,68,17},"Passport"},
         {box,{165,472,5,13},"/"},
         {box,{176,472,78,16},"Passeport"},
         {box,{317,474,55,17},"identity"},
         {box,{378,475,33,13},"card"},
         {box,{417,475,4,13},"/"},
         {box,{427,475,42,14},"Carte"},
         {box,{475,476,68,15},"d'identitÃ©"},
         {box,{618,478,53,13},"Other"},
         {box,{666,474,7,25},"/"},
         {box,{676,479,41,12},"Autre"},
         {box,{91,504,24,13},"Ne"},
         {box,{338,507,117,15},"AUTORIDADE"},
         {box,{461,509,93,15},"EMISSORA"},
         {box,{339,527,54,18},"Issuing"},
         {box,{399,529,69,17},"Authority"},
         {box,{473,530,5,13},"#"},
         {box,{484,530,52,14},"DeliveÃ©"},
         {box,{542,535,27,13},"par"},
         {box,{892,508,93,15},"VALIDADE"},
         {box,{676,174,489,583},"="},
         {box,{416,575,106,15},"FAMILIARES"},
         {box,{527,578,5,13},"/"},
         {box,{537,578,119,14},"DEPENDENTS"},
         {box,{661,579,6,13},"/"},
         {box,{672,579,119,13},"DEPENDANTS"},
         {box,{1127,564,38,36},"="},
         {box,{84,941,37,13},"Date"},
         {box,{126,941,16,13},"of"},
         {box,{146,943,39,15},"entry"},
         {box,{190,943,5,11},"/"},
         {box,{201,942,37,13},"Date"},
         {box,{243,943,64,13},"d'entrÃ©e"},
         {box,{84,982,40,17},"PAIS"},
         {box,{130,986,24,13},"DE"},
         {box,{160,983,109,17},"RESIDENCIA"},
         {box,{84,1049,78,13},"MORADA"},
         {box,{168,1049,24,13},"DE"},
         {box,{198,1049,99,14},"CONTACTO"},
         {box,{304,1051,26,13},"EM"},
         {box,{336,1051,100,13},"PORTUGAL,"},
         {box,{84,1000,80,27},"Permanent"},
         {box,{174,1000,53,27},"Adress"},
         {box,{235,1006,5,14},"/"},
         {box,{246,1006,38,17},"Pays"},
         {box,{290,1004,20,16},"de"},
         {box,{316,1005,76,16},"residence"},
         {box,{537,1008,53,17},"Length"},
         {box,{596,1008,15,13},"of"},
         {box,{615,1009,31,16},"stay"},
         {box,{651,1008,5,14},"/"},
         {box,{662,1008,36,14},"OurÃ©"},
         {box,{704,1009,18,13},"du"},
         {box,{728,1009,50,17},"sÃ©jour"},
         {box,{89,622,86,16},"GONJUGE"},
         {box,{719,634,34,12},"Doc."},
         {box,{760,633,27,13},"NÂ°."},
         {box,{88,645,59,16},"Spouse"},
         {box,{152,646,5,13},"/"},
         {box,{163,646,64,17},"Conjoint"},
         {box,{87,675,64,14},"FILHOS"},
         {box,{718,684,35,14},"Doc."},
         {box,{759,685,28,13},"NY."},
         {box,{87,696,64,13},"Children"},
         {box,{157,697,5,12},"/"},
         {box,{168,697,59,14},"Enfants"},
         {box,{556,736,4,3},":"},
         {box,{718,733,34,14},"Doc."},
         {box,{759,733,27,13},"N.Â°."},
         {box,{717,780,34,13},"Doc."},
         {box,{758,780,27,13},"N.Â°."},
         {box,{717,825,34,13},"Doc."},
         {box,{758,826,27,13},"NÂ°."},
         {box,{56,853,8,32},"["},
         {box,{481,868,94,14},"DECLARA/"},
         {box,{581,869,77,14},"Declares"},
         {box,{653,860,6,32},"/"},
         {box,{664,869,58,14},"Declare"},
         {box,{1127,865,37,44},"_]"},
         {box,{84,921,49,13},"DATA"},
         {box,{139,921,23,13},"DE"},
         {box,{168,921,87,14},"ENTRADA"},
         {box,{261,922,25,13},"EM"},
         {box,{292,923,100,13},"PORTUGAL"},
         {box,{605,924,40,16},"PAIS"},
         {box,{651,927,23,13},"DE"},
         {box,{680,925,133,17},"PROVENIENGIA"},
         {box,{537,986,89,19},"DURAGAO"},
         {box,{632,989,24,13},"DA"},
         {box,{662,989,71,14},"ESTADA"},
         {box,{739,990,24,13},"EM"},
         {box,{770,990,99,15},"PORTUGAL"},
         {box,{869,1009,164,8},
          [226,128,148,226,128,148,226,128,148,226,128,148,226,128,
           148,226,128,148,95,226,128,148,95,45,95,95]},
         {box,{605,945,60,17},"Country"},
         {box,{671,946,15,13},"of"},
         {box,{690,949,95,13},"provenience"},
         {box,{790,948,4,12},"/"},
         {box,{800,948,39,16},"Pays"},
         {box,{845,948,19,13},"de"},
         {box,{870,952,96,13},"provenance"},
         {box,{1037,996,42,15},"DIAS"},
         {box,{1037,1017,40,18},"Days"},
         {box,{1083,1019,6,14},"/"},
         {box,{1094,1021,46,14},"jours"},
         {box,{715,1091,33,14},"Teif."},
         {box,{84,1153,53,13},"DATA:"},
         {box,{424,1164,3,5},","},
         {box,{541,1155,19,13},"de"},
         {box,{84,1173,36,14},"Date"},
         {box,{542,1217,113,13},"ASSINATURA,"},
         {box,{513,1237,75,17},"Signature"},
         {box,{592,1237,6,14},"/"},
         {box,{602,1238,83,14},"Assinature"},
         {box,{870,1159,19,13},"de"},
         {box,{900,1160,29,13},"200"}],
    format_rects(Rects).

format_rects([]) ->
    ok;
format_rects([{box, {X, Y, Width, Height}, Key}|Rest]) ->
    io:format("\"~s\": {\"x\": ~w.0, \"y\": ~w.0, \"width\": ~w.0, \"height\": ~w},\n",
              [Key, X, Y, Width, Height]),
    format_rects(Rest).