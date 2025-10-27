vannnet_type <- read.table(sep="\t",header=T,text="
VannforekomstID	Vannforekomstnavn	Vannkategori	Vannregion	Nasjonal vanntype	Kystrev vanntype	Authority
0110000032-2-C	Stanggapet	Kystvann	Agder	S1		
0110000037-1-C	Rødskjærgapet - Vest	Kystvann	Agder	S1		
0110000701-C	Stølefjorden	Kystvann	Agder	S3		
0110000702-C	Haslumkilen	Kystvann	Agder			
0120000030-C	Ellingsvikskjer - Sildodden	Kystvann	Agder	S1		
0120000031-1-C	Sildodden - Flostaøya	Kystvann	Agder	S1		
0120000031-2-C	Gåsholmen - Kalvøya	Kystvann	Agder	S2		
0120000031-3-C	Sandøyfjorden	Kystvann	Agder			
0120000032-2-C	Klove-Arendal - Tromøy	Kystvann	Agder	S2		
0120000032-3-C	Arendal - Tromøy	Kystvann	Agder	S1		
0120000032-4-C	Arendal - Tromøy	Kystvann	Agder	S1		
0120000033-C	Alvekilen	Kystvann	Agder	S3		
0120000034-C	Merdø - Hasseltangen	Kystvann	Agder	S2		
0120010101-1-C	Søndeledfjorden	Kystvann	Agder			
0120010102-C	Østre Nordfjorden	Kystvann	Agder			
0120010201-C	Sørfjorden-vestre	Kystvann	Agder			
0120010202-C	Sørfjorden-østre	Kystvann	Agder			
0120010300-1-C	Kranfjorden	Kystvann	Agder	S3		
0120010300-3-C	Søndeledfjord-ytre	Kystvann	Agder	S2		
0120010300-4-C	Østerfjorden	Kystvann	Agder	S2		
0120010400-C	Risørleia	Kystvann	Agder	S2		
0120010502-1-C	Sandesfjorden	Kystvann	Agder			
0120010502-2-C	Avreidkilen	Kystvann	Agder			
0120020100-1-C	Lyngørfjorden-østre	Kystvann	Agder	S3		
0120020100-2-C	Lyngørfjorden-vestre	Kystvann	Agder			
0120020100-3-C	Breiungen	Kystvann	Agder			
0120020201-C	Sandøyfjorden	Kystvann	Agder			
0120020202-C	Kobbervika	Kystvann	Agder			
0120020203-C	Kåkvåg	Kystvann	Agder			
0120020300-C	Hagefjorden	Kystvann	Agder	S3		
0120020400-1-C	Oksefjorden-ytre	Kystvann	Agder			
0120020400-2-C	Oksefjorden-indre	Kystvann	Agder			
0120020502-C	Kvåstadkilen	Kystvann	Agder			
0120020600-C	Eikelandsfjorden	Kystvann	Agder			
0120030100-C	Flosterfjorden	Kystvann	Agder			
0120030201-1-C	Hasteinsundet	Kystvann	Agder	S3		
0120030203-1-C	Galtesund	Kystvann	Agder	S3		
0120030204-C	Merdøfjorden - Havsøyfjorden	Kystvann	Agder	S3		
0120030300-C	Hølen	Kystvann	Agder			
0120030400-C	Nidelven	Kystvann	Agder	S5		
0120030500-1-C	Nidelven-ytre	Kystvann	Agder	S3		
0120030500-2-C	Sømskilen-Hasseltangen	Kystvann	Agder	S3		
0120030600-1-C	Hovekilen-ytre	Kystvann	Agder	S3		
0120030600-2-C	Hovekilen-indre	Kystvann	Agder			
0121000030-C	Grimstad-ytre	Kystvann	Agder	S1		
0121000031-C	Hesneskanalen - Hasseltangen	Kystvann	Agder	S2		
0121000032-C	Blindleia - Skagerak	Kystvann	Agder	S1		
0121000033-C	Ramsøysund	Kystvann	Agder	S2		
0121000100-1-C	Fevikkilen, indre	Kystvann	Agder	S3		
0121000100-2-C	Fevikkilen, ytre	Kystvann	Agder	S3		
0121000300-1-C	Groosfjorden-indre	Kystvann	Agder			
0121000300-2-C	Groosfjord-ytre	Kystvann	Agder			
0121000300-3-C	Grimstad	Kystvann	Agder			
0121000400-C	Morvigkilen	Kystvann	Agder	S3		
0121000500-C	Strandfjorden	Kystvann	Agder	S5		
0121000600-C	Nørholmskilen	Kystvann	Agder	S3		
0121000700-C	Bufjorden	Kystvann	Agder	S3		
0121000800-C	Engekilen	Kystvann	Agder			
0121010100-C	Homborsund	Kystvann	Agder	S3		
0121010200-C	Vestre Bjørkesund - Sønningdalsund	Kystvann	Agder			
0121010300-C	Kaldvellfjorden	Kystvann	Agder			
0121010400-1-C	Lillesandsfjord-Klingsundkilen	Kystvann	Agder	S2		
0121010400-2-C	Lillesandsfjord-ytre	Kystvann	Agder	S2		
0121010500-2-C	Skallefjorden	Kystvann	Agder			
0121020100-C	Blindleia - Nesefjorden	Kystvann	Agder			
0121020201-C	Steindalsfjorden	Kystvann	Agder			
0121020202-C	Steindalsfjord-ytre	Kystvann	Agder			
0121020300-C	Blindleia - Kjøbmannsvig	Kystvann	Agder	S3		
0121020400-C	Røynevardsfjorden	Kystvann	Agder	S3		
0121020500-1-C	Bliksfjorden	Kystvann	Agder	S3		
0121020500-2-C	Blindleia - Hellesund	Kystvann	Agder	S2		
0121020600-1-C	Isefjærfjorden	Kystvann	Agder			
0121020600-2-C	Isefjærfjorden - Knabesund	Kystvann	Agder			
0121020700-C	Heslevigen	Kystvann	Agder	S3		
0130000030-1-C	Kristiansandsfjorden-ytre	Kystvann	Agder	S1		
0130000030-2-C	Kristiansandsfjorden - Mebø	Kystvann	Agder	S2		
0130000031-1-C	Vestergapet-ytre	Kystvann	Agder	S1		
0130000031-3-C	Kilen	Kystvann	Agder	S2		
0130000031-4-C	Herøyfjorden - Sandvikdalsfjorden	Kystvann	Agder	S2		
0130000101-C	Kvåsefjorden-indre	Kystvann	Agder			
0130000102-1-C	Kvåsefjorden - Kvarenesfjorden	Kystvann	Agder	S3		
0130000102-2-C	Kvåsefjorden-ytre	Kystvann	Agder	S2		
0130000200-1-C	Ytre Sandvikdalsfjorden	Kystvann	Agder	S3		
0130000200-2-C	Indre Sandvikdalsfjorden	Kystvann	Agder	S3		
0130000300-C	Hellesundfjorden	Kystvann	Agder	S3		
0130010203-C	Stølekilen	Kystvann	Agder			
0130010500-C	Ålefjærfjorden	Kystvann	Agder			
0131000030-C	Songvårfjorden - Hellefjorden	Kystvann	Agder	S1		
0131010100-1-C	Hundsøyfjorden	Kystvann	Agder	S3		
0131010200-C	Høllefjorden	Kystvann	Agder	S3		
0131010300-C	Trysfjorden	Kystvann	Agder			
0131010500-C	Dalskilen	Kystvann	Agder	S3		
0131020100-C	Harkmarkfjorden	Kystvann	Agder			
0131020201-C	Buøysund-indre	Kystvann	Agder	S3		
0131020202-C	Buøysund-ytre	Kystvann	Agder	S3		
0131020300-C	Hellefjorden	Kystvann	Agder	S3		
0131020400-C	Skjernøysundet	Kystvann	Agder			
0132000030-C	Mandal-Lindesnes	Kystvann	Agder	S1		
0132010201-C	Skogsfjord-indre	Kystvann	Agder			
0132010202-C	Skogsfjord-ytre	Kystvann	Agder	S3		
0132010300-C	Hillevågen	Kystvann	Agder	S3		
0132010400-C	Kåfjorden	Kystvann	Agder			
0132030100-C	Tjømsvågen	Kystvann	Agder	S3		
0132030200-C	Sniksfjorden	Kystvann	Agder			
0132030400-C	Remefjorden	Kystvann	Agder			
0132030500-C	Njervefjorden	Kystvann	Agder	S3		
0132030600-C	Kirkevågen	Kystvann	Agder	S3		
0132030700-C	Ramslandsvågen	Kystvann	Agder			
0201000030-3-C	Lindesnes - Lista	Kystvann	Agder	N1		
0201010100-C	Lenefjorden	Kystvann	Agder			
0201010200-C	Grønsfjorden	Kystvann	Agder	N4		
0201010300-C	Rosfjorden	Kystvann	Agder	N4		
0201010400-C	Skarvøy	Kystvann	Agder	N2		
0201010500-C	Spindfjorden	Kystvann	Agder			
0201010600-C	Indre Spindsfjorden	Kystvann	Agder	N4		
0201010700-1-C	Indre Spindsfjorden - Håøysundet	Kystvann	Agder	N3		
0201010700-2-C	Indre Spindsfjorden - Farsund	Kystvann	Agder	N3		
0201010800-C	Lyngdalsfjord-ytre	Kystvann	Agder			
0201010900-C	Lyngdalsfjord-indre	Kystvann	Agder	N5		
0201011000-C	Åptafjorden	Kystvann	Agder			
0201011100-C	Drangsfjorden	Kystvann	Agder			
0201011200-C	Helvikfjorden	Kystvann	Agder	N4		
0201011300-C	Framvaren	Kystvann	Agder			
0201020100-1-C	Listafjorden	Kystvann	Agder	N2		
0201020100-2-C	Listafjord - Kinna	Kystvann	Agder	N3		
0201020200-C	Eidsfjorden	Kystvann	Agder	N2		
0201020302-C	Fedafjord-ytre	Kystvann	Agder	N5		
0201020400-C	Stolsfjorden	Kystvann	Agder	N3		
0201020600-C	Grisefjorden	Kystvann	Agder			
0201020700-1-C	Logakanalen	Kystvann	Agder	N5		
0201020800-C	Hidrasundet	Kystvann	Agder	N2		
0201020900-C	Rasvågen	Kystvann	Agder	N3		
0201021000-C	Berrefjord	Kystvann	Agder	N4		
0201021100-C	Ånafjorden	Kystvann	Agder	N5		
0101000032-2-C	Vauerkilen	Kystvann	Glomma	S3	S02	AS
0101000032-4-C	Nordre og Søndre Søster	Kystvann	Glomma	S1	S05	AS
0101010201-C	Iddefjorden-indre	Kystvann	Glomma	S5	S03	AS
0101010302-C	Svalerødkilen	Kystvann	Glomma	S3	S02	AS
0101010303-C	Røsneskilen	Kystvann	Glomma	S3	S02	AS
0101010304-C	Grimsøykilen	Kystvann	Glomma	S3	S02	AS
0101010305-C	Skjebergkilen	Kystvann	Glomma	S3	S02	AS
0101010306-C	Sandholmene	Kystvann	Glomma	S3	S02	AS
0101010402-C	Tosekilen	Kystvann	Glomma	S3	S02	AS
0101010406-C	Skjelsbusundet	Kystvann	Glomma	S3	S02	AS
0101010407-C	Asmalsundet	Kystvann	Glomma	S3	S02	AS
0101010408-C	Løperen	Kystvann	Glomma	S3	S02	AS
0101010500-C	Lera	Kystvann	Glomma	S1	S03	AS
0101020101-2-C	Ytre Oslofjord - Øst	Kystvann	Glomma	S2	S05	AS
0101020102-1-C	Hankøsundet	Kystvann	Glomma	S3	S02	AS
0101020102-2-C	Risholmsundet	Kystvann	Glomma	S3	S02	AS
0101020103-C	Elingårdskilen	Kystvann	Glomma	S3	S02	AS
0101020104-C	Krokstadfjorden	Kystvann	Glomma	S3	S02	AS
0101020105-C	Kurefjorden	Kystvann	Glomma	S3	S02	AS
0101020200-1-C	Midtre Oslofjord - Øst	Kystvann	Glomma	S2	S05	AS
0101020300-2-C	Breiangen-øst	Kystvann	Glomma	S2	S05	AS
0101020500-C	Hurum	Kystvann	Glomma	S3	S02	AS
0101020603-C	Holmenfjorden	Kystvann	Glomma	S3	S02	AS
0101021000-1-C	Breiangen-vest	Kystvann	Glomma	S3	S02	AS
0101020802-C	Drammensfjorden-ytre	Kystvann	Vest-Viken	S3	S03	AS
0110000032-1-C	Jomfruland-sørvest	Kystvann	Vest-Viken	S1		
0110000034-1-C	Såsteinflaket - Steinrenna	Kystvann	Vest-Viken	S1		
0110000034-2-C	Såsteinflaket	Kystvann	Vest-Viken	S2		
0110000035-C	Skrurenna	Kystvann	Vest-Viken	S1		
0110000037-2-C	Rødskjærgapet - Øst	Kystvann	Vest-Viken	S1		
0110000038-1-C	Melbyfjorden	Kystvann	Vest-Viken	S2		
0110000038-2-C	Prisgrunnbukta	Kystvann	Vest-Viken			
0110000300-C	Rognsfjorden	Kystvann	Vest-Viken	S2		
0110000400-C	Åbyfjorden	Kystvann	Vest-Viken	S3		
0110000500-C	Brevikstrandfjorden	Kystvann	Vest-Viken	S3		
0110000600-C	Trosbyfjorden	Kystvann	Vest-Viken	S3		
0110010401-C	Ønna	Kystvann	Vest-Viken	S5		
0110010702-C	Gunnekleivfjorden	Kystvann	Vest-Viken			
0110020100-C	Eksefjorden	Kystvann	Vest-Viken	S2		
0110020200-C	Fossingfjorden	Kystvann	Vest-Viken	S3		
0110020300-C	Kjøpmannsfjorden	Kystvann	Vest-Viken	S3		
0110020400-C	Soppekilen	Kystvann	Vest-Viken	S3		
0110020500-C	Langårdssund	Kystvann	Vest-Viken	S3		
0110020600-C	Oterøyfjorden	Kystvann	Vest-Viken	S3		
0110020700-C	Skåtøysund	Kystvann	Vest-Viken	S3		
0110021000-2-C	Kragerøfjorden-indre	Kystvann	Vest-Viken	S3		
0110021101-1-C	Kilsfjorden	Kystvann	Vest-Viken			
0110021103-C	Barlandkilen	Kystvann	Vest-Viken	S5		
0110021200-C	Skåtøybukta	Kystvann	Vest-Viken	S2		
0132010100-C	Mannefjorden	Kystvann	Agder	S3		
0131010400-C	Kumlefjorden	Kystvann	Agder	S3		
0132030300-C	Syrdalsfjorden	Kystvann	Agder	S3		
0131010100-2-C	Hundsøyfjorden-ytre	Kystvann	Agder	S2		
0131010100-3-C	Torvefjorden	Kystvann	Agder	S3		
0201000031-C	Flekkefjord-ytre	Kystvann	Agder	N1		
0201000030-2-C	Lindesnes - Lista	Kystvann	Agder	N2		
0201011400-C	Lundevågen	Kystvann	Agder	N3		
0201020301-C	Fedafjord-indre	Kystvann	Agder	N5		
0201020500-1-C	Lafjord	Kystvann	Agder	N4		
0201020500-2-C	Tjørsvågbukta	Kystvann	Agder	N4		
0130010301-2-C	Østergapet-indre	Kystvann	Agder	S2		
0130010302-2-C	Kristiansandsfjorden-indre havn	Kystvann	Agder	S3		
0130010302-3-C	Kristiansandsfjorden-indre	Kystvann	Agder	S3		
0130010400-1-C	Topdalsfjorden-indre	Kystvann	Agder			
0130010600-C	Drangsvatna	Kystvann	Agder			
0130010700-C	Vestergapet-indre	Kystvann	Agder	S3		
0130010202-C	Randøysund-indre	Kystvann	Agder	S3		
0130010301-1-C	Østergapet-ytre	Kystvann	Agder	S3		
0130010400-2-C	Topdalsfjorden-ytre	Kystvann	Agder			
0130010400-3-C	Kongsgårdbukta - Marviksbukta	Kystvann	Agder			
0130010201-C	Randøysund-ytre	Kystvann	Agder	S3		
0101010202-2-C	Halden havnebasseng	Kystvann	Glomma	S5	S02	AS
0101010401-C	Ramsøflaket - Østerelva	Kystvann	Glomma	S3	S03	AS
0101010403-C	Hunnebunn	Kystvann	Glomma	S5	S02	AS
0101010405-C	Østerelva	Kystvann	Glomma	S5	S05	AS
0101020300-1-C	Hårfagrebåen - Hortenskrakken	Kystvann	Vest-Viken	S2	S05	AS
0101020400-2-C	Mossesundet- indre	Kystvann	Glomma	S3	S02	AS
0101020400-3-C	Mossesundet-ytre	Kystvann	Glomma	S3	S02	AS
0101020602-C	Bærumsbassenget	Kystvann	Glomma	S5	S02	AS
0101020701-5-C	Bunnefjorden	Kystvann	Glomma	S3	S02	AS
0101020701-6-C	Bunnebotn	Kystvann	Glomma	S5	S03	AS
0101020702-1-C	Oslo havn og by	Kystvann	Glomma	S3	S02	AS
0101020702-2-C	Bekkelagsbassenget	Kystvann	Glomma	S3	S02	AS
0101020801-C	Drammensfjorden-indre	Kystvann	Vest-Viken	S5	S03	AS
0101021000-2-C	Langøya	Kystvann	Vest-Viken	S3	S02	AS
0101021100-C	Horten indre havn	Kystvann	Vest-Viken	S3	S02	AS
0110000031-C	Jomfruland-nordøst	Kystvann	Vest-Viken	S1		
0110000036-C	Jomfrulandsrenna	Kystvann	Vest-Viken	S3		
0110010703-C	Voldsfjorden	Kystvann	Vest-Viken	S5		
0110020800-1-C	Bærøyfjorden - Skarholmane	Kystvann	Vest-Viken	S3		
0110020800-2-C	Bærøyfjorden - Kragerø	Kystvann	Vest-Viken	S3		
0110020900-C	Hellefjorden	Kystvann	Vest-Viken			
0110021000-1-C	Kragerøfjorden-ytre	Kystvann	Vest-Viken	S3		
0110021101-2-C	Kilsfjorden - Høyåsen	Kystvann	Vest-Viken	S3		
0110021300-C	Kalstadkilen	Kystvann	Vest-Viken	S3		
0120010101-2-C	Østre Nordfjorden	Kystvann	Agder			
0120020501-C	Tvedestrandsfjorden	Kystvann	Agder			
0120030201-2-C	Tromøysundet	Kystvann	Agder	S3		
0120030202-1-C	Neskilen - Rånehølen	Kystvann	Agder	S5		
0120030202-2-C	Tromøysundet - Bekkevika	Kystvann	Agder	S3		
0120030203-2-C	Tromøysund - Arendal	Kystvann	Agder	S3		
0121000200-C	Vikkilen	Kystvann	Agder			
0121010500-1-C	Lillesandsfjorden	Kystvann	Agder	S3		
0101030200-C	Husøyflaket	Kystvann	Vest-Viken	S3	S02	AS
0101030300-C	Huikjæla	Kystvann	Vest-Viken	S2	S02	AS
0101030400-C	Årøysund - Hvasser	Kystvann	Vest-Viken	S2	S02	AS
0101030500-C	Sandøsund	Kystvann	Vest-Viken	S3	S02	AS
0101030600-C	Tjøme - Holtekjærkilen	Kystvann	Vest-Viken	S3	S02	AS
0101030700-C	Vrengen	Kystvann	Vest-Viken	S3	S02	AS
0101020900-C	Sandebukta	Kystvann	Vest-Viken	S3	S02	AS
0101030101-1-C	Træla	Kystvann	Vest-Viken	S5	S02	AS
0101030101-5-C	Vestfjorden-nordre	Kystvann	Vest-Viken	S5	S02	AS
0101030101-6-C	Vestfjorden-søndre	Kystvann	Vest-Viken	S3	S02	AS
0101020101-1-C	Ytre Oslofjord - Vest	Kystvann	Vest-Viken	S2	S05	AS
0101020200-2-C	Midtre Oslofjord - Vest	Kystvann	Vest-Viken	S2	S05	AS
0101000030-2-C	Færder	Kystvann	Vest-Viken	S1	S06	AS
0101030101-2-C	Kanalen	Kystvann	Vest-Viken	S5	S02	AS
0101030101-3-C	Byfjorden	Kystvann	Vest-Viken	S5	S02	AS
0101040300-3-C	Larviksfjorden	Kystvann	Vest-Viken	S2	S05	AS
0101040300-4-C	Stavern	Kystvann	Vest-Viken	S3	S02	AS
0101030102-1-C	Tønsbergfjorden - Tjømekjæla	Kystvann	Vest-Viken	S3	S02	AS
0101030102-2-C	Tønsbergfjorden-ytre	Kystvann	Vest-Viken	S2	S04	AS
0101040200-1-C	Sandefjordsfjorden-indre	Kystvann	Vest-Viken	S3	S02	AS
0101040200-2-C	Sandefjordsfjorden-ytre	Kystvann	Vest-Viken	S3	S04	AS
0101000031-C	Svenner - Rauer	Kystvann	Vest-Viken	S1	S06	AS
0101000032-3-C	Tjøme	Kystvann	Vest-Viken	S1	S05	AS
0101040400-3-C	Viksfjorden indre	Kystvann	Vest-Viken	S3	S02	AS
0101040400-4-C	Viksfjorden	Kystvann	Vest-Viken	S3	S02	AS
0101040500-C	Naverfjorden	Kystvann	Vest-Viken	S2		
0110000030-C	Langesundsbukta-ytre	Kystvann	Vest-Viken	S1		
0110000033-1-C	Langesundsbukta-indre	Kystvann	Vest-Viken	S1		
0110000033-2-C	Langesundsbukta - Rognsfjorden	Kystvann	Vest-Viken	S1		
0110010100-C	Helgeroafjorden	Kystvann	Vest-Viken	S2		
0110010200-C	Mørjefjorden	Kystvann	Vest-Viken	S3		
0110010300-C	Håøyafjorden	Kystvann	Vest-Viken	S3		
0110010402-C	Langangsfjorden	Kystvann	Vest-Viken	S3		
0110010801-C	Langesundsfjorden	Kystvann	Vest-Viken	S3		
0110010802-C	Langesundsfjorden - Dypingen	Kystvann	Vest-Viken			
0110010500-C	Ormefjorden	Kystvann	Vest-Viken	S3		
0110010600-C	Eidangerfjorden	Kystvann	Vest-Viken	S3		
0110000100-C	Hummerbakkfjorden	Kystvann	Vest-Viken	S2		
0110000200-C	Nevlunghavn - Havnebukta	Kystvann	Vest-Viken	S2		
0101020601-C	Oslofjorden	Kystvann	Glomma	S2	S02	AS
0110021102-C	Kjølebrønnskilen	Kystvann	Vest-Viken			
0110010701-C	Frierfjorden	Kystvann	Vest-Viken			
0101030800-C	Røssesundet	Kystvann	Vest-Viken	S3	S02	AS
0101040100-C	Mefjorden	Kystvann	Vest-Viken	S3	S04	AS
0101010301-C	Singlefjorden	Kystvann	Glomma	S3	S02	AS
0101000030-1-C	Torbjørnskjær	Kystvann	Glomma	S1	S07	AS
0101010202-1-C	Iddefjorden hovedbasseng	Kystvann	Glomma	S5	S02	AS
0101010203-C	Iddefjorden-ytre	Kystvann	Glomma		S02	AS
")

vannnet_type <- vannnet_type %>%
  select(Vannforeko=VannforekomstID, 
         #Vannfore_1=Vannforekomstnavn,
         Type=Nasjonal.vanntype,
         TypeKR=Kystrev.vanntype,
         Authority)%>% 
  mutate(Type = ifelse(Type=="","unknown",Type),
         TypeKR = ifelse(TypeKR=="","unknown",TypeKR))
