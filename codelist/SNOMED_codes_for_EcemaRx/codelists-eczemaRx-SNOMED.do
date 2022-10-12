/*=========================================================================
Updated by Julian Matthewman 17/05/2021 (see UPDATED FROM HERE)
*=========================================================================*/


/*=========================================================================
DO FILE NAME:			alspac-codelist01v2-eczemaRx

AUTHOR:					Kate Mansfield	
						Adapted from HF CVD eczema study code: 
							cr_eczema_therapy_v2.do
						Adpted from AE cancer/mortality/anxdep/fracture code:
							exca-codelist01v2-eczemaRx
						
VERSION:				v2
DATE VERSION CREATED: 	2020-Mar-11 // not a major update, just a long time between work!
						2019-Oct-25
					
DATABASE:				
	
DESCRIPTION OF FILE:	Aim: identify SNOMED codes for eczema
						Based on search strategy originally used to identify
						CPRD product codes.
						 				
MORE INFORMATION:	
	
DATASETS USED:		SNOMED_Rx_incBNF

DATASETS CREATED: 	snomed-eczemaRx		// snomed product codes for eczema treatment
																
DO FILES NEEDED:	alspac-paths.do

ADO FILES NEEDED: 	alspac.ado

*=========================================================================*/

/*******************************************************************************
>> HOUSEKEEPING
*******************************************************************************/
version 15
clear all
capture log close

* find path file location and run it
skinepipaths

* create a filename global that can be used throughout the file
global filename "codelist-eczemaRx-SNOMED"

* open log file - no need as fast tool will create log files
log using "${pathLogs}/${filename}", text replace





/*******************************************************************************
#1. Load SnomedCT UK Drug dataset
*******************************************************************************/
use "${pathJdrive}/data_in/SNOMED_Rx_incBNF", clear


* rename some vars to give shorter names
rename MDRProductDescription mdrProdDesc
rename DMDProductDescription dmdProdDesc
rename DMDProductandPackDescripti dmdProdPackDesc
rename BNFChapter bnfchap
rename BNFSection bnfsec
rename BNFParagraph bnfpara
rename BNFSubparagraph bnfsubpara
rename BNFChemicalSubstance bnfsubst
rename BNFProduct bnfprod
rename BNFPresentation bnfpres



* create lower case versions of all the variables to search
local searchVars " "mdrProdDesc" "dmdProdDesc" "dmdProdPackDesc" "bnfchap" "
local searchVars " `searchVars' "bnfsec" "bnfpara" "bnfsubpara" "bnfsubst" "bnfprod" "bnfpres" "


foreach x in `searchVars' {
	generate lc_`x'=lower(`x')
} 

order SNOMEDCode lc_*






/*******************************************************************************
#2. Define search terms
*******************************************************************************/
/*Treatment atopic eczema in adults:
	- Mild flares: emollients and mild topical hydrocortisone cream or ointment
		(such as hydrocortisone 1%) 
	- Moderate flares: emollients and moderate topical hydrocortisone cream or 
		ointment (for example betamethasone valerate 0.025% or clobetasone butyrate 0.05%).
	- For delicate areas, prescribe mild potency topical corticosteroid (such as 
		hydrocortisone 1%) and increase to a moderate potency corticosteroid only if necessary.
	- If infected, use topical antibiotic
	- Severe eczema: a potent topical corticosteroid for inflamed areas 
		(for example betamethasone valerate 0.1%).
		For delicate areas of skin such as the face and flexures, use a moderate potency 
		corticosteroid (such as betamethasone valerate 0.025%, clobetasone butyrate 0.05%). 
		If there is severe, extensive eczema causing psychological distress, 
		consider prescribing a short course of oral corticosteroids
	If infected, use oral antibiotic

*/






/*------------------------------------------------------------------------------
#2.1 Emolients
------------------------------------------------------------------------------*/
local emo " 		"*aqueous cream*" "*emollient*" "*moisturis*" "*bath oil*" "
local emo "`emo' 	"*liquid paraffin*"   "*isopropyl myristate*"   "*glycerol*"  "*wool fat*" "
local emo "`emo' 	"*lanolin*"   "*lauromacrogols*"   "*soya oil*"   "*emulsifying wax*"  "
local emo "`emo' 	"*emulsifying ointment*"   "*soft paraffin*"   "*coconut oil*"   "*urea*" "
local emo "`emo' 	"*sodium pyrrolidone carboxylate*"   "*glycerol*"   "*hydrous ointment*"  "
local emo "`emo' 	"*essential fatty acids*"   "*evening primrose oil*"   "*allantoin*"   "
local emo "`emo' 	"*sodium pidolate*"   "*aquamax*"   "*aquamol*"   "*aveeno*"   "*cetraben*"  "
local emo "`emo' 	"*dermamist*"   "*diprobase*"   "*doublebase*"   "*e45*"   "*emollin*"  "
local emo "`emo' 	"*epaderm*"   "*hydromol*"   "*lipobase*"   "*oilatum*"   "*qv*"  "
local emo "`emo' 	"*ultrabase*"   "*unguentum m*"   "*zeroaqs*"   "*zerobase*"  "
local emo "`emo' 	"*zerocream*"   "*zeroderm*"   "*zeroguent*"   "*aquadrate*"  "
local emo "`emo' 	"*balneum*"   "*calmurid*"   "*e45*"   "*eucerin intensive*" "
local emo "`emo' 	"*flexitol*"   "*hydromol intensive*"   "*imuderm*"   "*nutraplus*"  "
local emo "`emo' 	"*emulsiderm*"   "*dermol*"   "*eczmol*"  "*petroleum*" "*dermalex*" "



/*------------------------------------------------------------------------------
#2.2 Topical steroids
------------------------------------------------------------------------------*/
local top_cs " 			"*stiedex*cream*" "*desoximetasone*" "*tridesilon*" "
local top_cs "`top_cs' 	"*fluclorolone*" "*methylprednisolone aceponate*" "*hydrocortisone*"  "
local top_cs "`top_cs' 	"*dioderm*"   "*mildison*"   "*synalar *"   "*canesten hc*"   "*daktacort*"  " 
local top_cs "`top_cs' 	"*econacort*"   "*fucidin h*"   "*nystaform-hc*"   "*terra-cortril*"  " 
local top_cs "`top_cs' 	"*timodine*"   "*betnovate-rd*"   "*eumovate*"   "*haelan*"   "*modrasone*" "  
local top_cs "`top_cs' 	"*ultralanum plain*"   "*trimovate*"   "*alphaderm*"   "
local top_cs "`top_cs' 	"*beclometasone dipropionate *"   "*betamethasone valerate*"   "*betacap*"   "
local top_cs "`top_cs' 	"*betesil*"   "*bettamousse*"   "*betnovate*"   "*cutivate*"   "*diprosone*"   "
local top_cs "`top_cs' 	"*elocon*"   "*hydrocortisone butyrate*"   "*locoid*"   "*locoid crelo*"   "
local top_cs "`top_cs' 	"*metosyn*"   "*mometasone furoate*"   "*nerisone*"   "*synalar*"  "
local top_cs "`top_cs' 	"*aureocort*"   "*betamethasone and clioquinol*"   "*betamethasone and neomycin*"  "
local top_cs "`top_cs' 	"*fucibet*"   "*lotriderm*"   "*synalar c*"   "*synalar n*"   "*diprosalic*"   "
local top_cs "`top_cs' 	"*clarelux*"   "*dermovate*"   "*etrivex*"   "*nerisone forte*" "
local top_cs "`top_cs' 	"*clobetasol with neomycin and nystatin*" "*alclometasone dipropionate*" "
local top_cs "`top_cs' 	"*hydrocortisone*" "*betamethasone*"   "*beclometasone*" "*budesonide*" "
local top_cs "`top_cs' 	"*fluocinolone acetonide*"   "*clobetasol propionate*" "*clobetasone butyrate*" "
local top_cs "`top_cs' 	"*diflucortolone valerate*"  "*triamcinolone*"  "*fludroxycortide*" "
local top_cs "`top_cs' 	"*desonide desoximetasone*"  "*fluocinonide*"  "*fluocortolone*" "
local top_cs "`top_cs' 	"*fluticasone*"  "*mometasone furoate*" "*methylprednisolone*" "*neo-medrone*" "


/*------------------------------------------------------------------------------
#2.3 Oral steroids
------------------------------------------------------------------------------*/
local oral_gc " 			"*pevanti*" "*plenadren*" "*cortisone acetate*" "*predniso*" "
local oral_gc "`oral_gc'	"*dexamethasone*"  "*deflazacort*" "*betamethasone*" "*betamethasome*"  "
local oral_gc "`oral_gc'	"*hydrocortisone*" "*cotisone acetate*" "*triamcinolone acetonide*"  "
local oral_gc "`oral_gc'	"*budesonide*" "*beclometasone dipropionate*" "*triamcinolone hexacetonide*" "
local oral_gc "`oral_gc'	"*fludrocortisone acetate*" "
local oral_gc "`oral_gc'	"*budesonide*" "*cortiment*" "*budenofalk*" "*entocort*" "*calcort*" "
local oral_gc "`oral_gc'	"*glensoludex*" "*neofordex*" "*dexsol*" "*fludrocortisone*" "
local oral_gc "`oral_gc'	"*medrone*" "*deltacortril*" "*dilacort*" "*prednisone*" "
local oral_gc "`oral_gc'	"*lodotra*" "


/*------------------------------------------------------------------------------
#2.4 Topical calcineurin inhibitors
------------------------------------------------------------------------------*/
local tcal " "*tacrolimus*" "*pimecrolimus*" "*elidel*" "*protopic*" " /*topical only*/
 

/*------------------------------------------------------------------------------
#2.5 Systemic drugs
------------------------------------------------------------------------------*/
local sys " 		"*ciclosporin*" "*cyclosporin*" "*neoral*" "*sandimmun*" " 	// ciclosporin
local sys "`sys'	"*vanquoral*" "*capimune*" "*capsorin*" "*deximune*"  "
local sys "`sys'	"*azathioprine*" "*imuran*" "*azapress*" "					// azathioprine
local sys "`sys'	"*methotrexate*" "*maxtrex*" "*jylamvo*" "*nordimet*"  "	// methotrexate
local sys "`sys'	"*zlatal*" "*methofill*" "*metoject*" "
local sys "`sys'	"*mycophenolate*" "*cellcept*" "*myfenax*" "				// mycophenolate






/*******************************************************************************
#3. Use search terms to identify relevant codes - based on search of 
		- medical device classification description
		- dm+d product description
		- dm+d product and pack description
		- BNF headers at the following levels: chapter, section, paragraph,
			subparagraph, subsection, product and presentation
*******************************************************************************/
* first create a marker variable for each type of therapy
foreach x in emo top_cs tcal oral_gc sys  {
	gen `x'=.
} /*end foreach x in emo top_cs tcal oral_gc sys*/

* identify vars to search
local term_lc " "lc_mdrProdDesc" "lc_dmdProdDesc" "lc_dmdProdPackDesc" "lc_bnfchap" " 
local term_lc " `term_lc' "lc_bnfsec" "lc_bnfpara" "lc_bnfsubpara" "lc_bnfsubst" "lc_bnfprod" "lc_bnfpres" "

* then update the marker where productname matches search terms
foreach x in emo top_cs tcal oral_gc sys  {
	foreach term in `term_lc' { // vars to search
		foreach word in ``x''{
           recode `x' .= 1 if strmatch(`term', "`word'")
         } /*end foreach word in ``x''*/
	} /*end foreach term in `term_lc'*/
} /*end foreach x in emo top_cs tcal oral_gc sys*/

label var emo "emolient"
label var top_cs "topical corticosteroid"
label var tcal "topical calcineurin inhibitor"
label var oral_gc "oral glucocorticoid"
label var sys "systemic drug"

* identify codes with one or more of the above markers for each therapy type
gen total=.
foreach x in emo top_cs tcal oral_gc sys  {
	recode total .=1 if `x'==1
} /*end foreach x in emo top_cs tcal oral_gc sys */	


label var total "identified in one or more cat of eczema med"

order lc_* emo top_cs tcal oral_gc sys tot









/*******************************************************************************
#4. Use bnf codes to identify futher codes
*******************************************************************************/	  
gen keep=.		  
recode keep .= 1 if BNFChapterCode==13 /*Preparations For Eczema And Psoriasis*/
recode keep .= 1 if BNFParagraphCode==80103 /*Antimetabolites*/
recode keep .= 1 if BNFParagraphCode==80201 /*antiproliferative immunosuppressants*/
recode keep .= 1 if BNFParagraphCode==80202 /*Corticosteroids and other immunosuppressants*/
		  
label var keep "possible eczema therapy"
recode keep .=1 if total==1
	


/*******************************************************************************
#5. Only keep products identified by the search
*******************************************************************************/	 
keep if keep==1 






/*******************************************************************************
#6. Review each category of eczema medication
	Exclude irrelevant stuff
*******************************************************************************/	 
/*------------------------------------------------------------------------------
#6.1 Emollients
------------------------------------------------------------------------------*/
* first take a look at stuff to identify things that might not be relevant
tab route if emo==1, miss sort f // route of admin
tab bnfchap if emo==1, sort f	// bnf headers
tab bnfsec if emo==1, sort f
tab bnfpara if emo==1, sort f
tab bnfsubpara if emo==1, sort f
tab bnfsubst if emo==1, miss sort f // substances


***************
* 6.1.1 ROUTE
***************
* identify and exclude emolients that are not administered topically
recode emo 1=. if (route=="Oral" | route=="Intravenous" ///
	| route=="Ocular" | route=="Rectal" | route=="Auricular" | route=="Intrathecal" ///
	| route=="Nasal" | route=="Oromucosal" | route=="Gastroenteral" | route=="Perineural")

tab route if emo==1, miss sort f // R/V >> looks OK



***************
* 6.1.2 REVIEW BY HIGH-LEVEL BNF CHAPTER
***************
* identify and exclude relevant medications
tab bnfchap if emo==1, sort f	// bnf headers

/*
                            BNF Chapter |      Freq.     Percent        Cum.
----------------------------------------+-----------------------------------
                                   Skin |      2,746       74.88       74.88
                             Appliances |        437       11.92       86.80
           Other Drugs And Preparations |        153        4.17       90.97
               Gastro-Intestinal System |        116        3.16       94.14
                              Dressings |         71        1.94       96.07
                     Respiratory System |         59        1.61       97.68
                                    Eye |         23        0.63       98.31
               Ear, Nose And Oropharynx |         21        0.57       98.88
                    Nutrition And Blood |         16        0.44       99.32
                       Stoma Appliances |         11        0.30       99.62
      Immunological Products & Vaccines |         10        0.27       99.89
                       Endocrine System |          4        0.11      100.00
----------------------------------------+-----------------------------------
                                  Total |      3,667      100.00
*/


* R/V drugs listed under chapters other than 'Skin'
* GI system
list lc_dmdProdDesc if bnfchap=="Gastro-Intestinal System" & emo==1

local excterms ""*glyceryl trinitrate*" "*suppositor*" "*liquid paraffin*" "*sucralfate*" "*allantoin*" "
foreach word in `excterms' {
	display "*** `word' ***"
	recode emo 1=. if strmatch(lc_dmdProdDesc, "`word'") & bnfchap=="Gastro-Intestinal System" & emo==1
} /*end foreach word in `excterms'*/

* Appliances
list lc_dmdProdDesc if bnfchap=="Appliances" & emo==1

local excterms ""*mouthwash*" "*mouth gel*" "*mouth spray*" "*diabetic foam cream*" "
local excterms "`exterms' "*clearzal*" "*nasal drops*" "
foreach word in `excterms' {
	display "*** `word' ***"
	recode emo 1=. if strmatch(lc_dmdProdDesc, "`word'") & bnfchap=="Appliances" & emo==1
} /*end foreach word in `excterms'*/

* most other things in this chapter seem fine

* Dressings
list lc_dmdProdDesc if bnfchap=="Dressings" & emo==1
recode emo 1=. if bnfchap=="Dressings" & emo==1

* Respiratory System
list lc_dmdProdDesc if bnfchap=="Respiratory System" & emo==1
recode emo 1=. if bnfchap=="Respiratory System" & emo==1

* Eye
list lc_dmdProdDesc if bnfchap=="Eye" & emo==1
recode emo 1=. if bnfchap=="Eye" & emo==1

* Ear, Nose And Oropharynx
list lc_dmdProdDesc if bnfchap=="Ear, Nose And Oropharynx" & emo==1
recode emo 1=. if bnfchap=="Ear, Nose And Oropharynx" & emo==1

* Other drugs and preparations
list lc_dmdProdDesc if bnfchap=="Other Drugs And Preparations" & emo==1
local excterms ""*aftersun*" "*urea powder*" "
foreach word in `excterms' {
	display "*** `word' ***"
	recode emo 1=. if strmatch(lc_dmdProdDesc, "`word'") & bnfchap=="Other Drugs And Preparations" & emo==1
} /*end foreach word in `excterms'*/

* Nutrition And Blood
list lc_dmdProdDesc if bnfchap=="Nutrition And Blood" & emo==1
recode emo 1=. if strmatch(lc_dmdProdDesc, "*injection*") & bnfchap=="Nutrition And Blood" & emo==1

* Stoma Appliances
list lc_dmdProdDesc if bnfchap=="Stoma Appliances" & emo==1
recode emo 1=. if strmatch(lc_dmdProdDesc, "*ostomy*") & bnfchap=="Stoma Appliances" & emo==1

* Immunological Products & Vaccines
list lc_dmdProdDesc if bnfchap=="Immunological Products & Vaccines" & emo==1
recode emo 1=. if bnfchap=="Immunological Products & Vaccines" & emo==1

* Endocrine System
list lc_dmdProdDesc if bnfchap=="Endocrine System" & emo==1
recode emo 1=. if bnfchap=="Endocrine System" & emo==1




* R/V
tab bnfchap if emo==1, sort miss f	// bnf headers





***************
* 6.1.3 REVIEW BY BNF CHAPTER SECTION
***************
tab bnfsec if emo==1, sort f

* topical CS
list lc_dmdProdDesc if bnfsec=="Topical Corticosteroids" & emo==1
	// these are all steroids carried in an emollient base
	// OK to leave
 
* Acne and Rosacea
list lc_dmdProdDesc if bnfsec=="Acne and Rosacea" & emo==1
	// active ingredients carried in emollient base drop
local excterms ""*salicylic*" "*sulfur*" "*clindamycin*" "
foreach word in `excterms' {
	display "*** `word' ***"
	recode emo 1=. if strmatch(lc_dmdProdDesc, "`word'") & bnfsec=="Acne and Rosacea" & emo==1
} /*end foreach word in `excterms'*/

* Top Local Anaesthetics & Antipruritics
list lc_dmdProdDesc if bnfsec=="Top Local Anaesthetics & Antipruritics" & emo==1
	// all menthol containing - drop
	recode emo 1=. if strmatch(lc_dmdProdDesc, "*menthol*") & bnfsec=="Top Local Anaesthetics & Antipruritics" & emo==1

* Selective Preparations
list lc_dmdProdDesc if bnfsec=="Selective Preparations" & emo==1
	// probably all fine leave

* Preparations For Warts And Calluses
list lc_dmdProdDesc if bnfsec=="Preparations For Warts And Calluses" & emo==1
	// largely contain salicylic acid >> drop
	recode emo 1=. if bnfsec=="Preparations For Warts And Calluses" & emo==1

* single substances
list lc_dmdProdDesc if bnfsec=="Single Substances" & emo==1
	// seems OK: glycerol liquid, glycerin liquid and liquid paraffin light liquid >> ask <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<

* Miscellaneous Topical Preparations
list lc_dmdProdDesc if bnfsec=="Miscellaneous Topical Preparations" & emo==1
local excterms ""*monobenzone*" "*hydroquinone*" "*tretinoin*" "
foreach word in `excterms' {
	display "*** `word' ***"
	recode emo 1=. if strmatch(lc_dmdProdDesc, "`word'") & bnfsec=="Miscellaneous Topical Preparations" & emo==1
} /*end foreach word in `excterms'*/

* Antiperspirants
list lc_dmdProdDesc if bnfsec=="Antiperspirants" & emo==1
recode emo 1=. if bnfsec=="Antiperspirants" & emo==1

*  Management of Skin Conditions
list lc_dmdProdDesc if bnfsec=="Management of Skin Conditions" & emo==1 // all good

* Vaginal Moisturisers
list lc_dmdProdDesc if bnfsec=="Vaginal Moisturisers" & emo==1
recode emo 1=. if bnfsec=="Vaginal Moisturisers" & emo==1

* Acids
list lc_dmdProdDesc if bnfsec=="Acids" & emo==1
	// according to Harriet's code, need to drop emollients with salicylic acid if not
	// prescribed with a top corticosteroid, will do this specifically later <<<<<<<<<<<<<<<<<<

* Skin Cleansers,Antiseptics & Deslough	
list lc_dmdProdDesc BNFSectionCode if strmatch(bnfsec, "Skin Cleansers,Antiseptics & Deslough*") & emo==1
recode emo 1=. if strmatch(lc_dmdProdDesc, "*methylated spirit*") & BNFSectionCode==1311 & emo==1
	
* Dry Mouth Products	
list lc_dmdProdDesc if bnfsec=="Dry Mouth Products" & emo==1
recode emo 1=. if bnfsec=="Dry Mouth Products" & emo==1

* Sunscreens And Camouflagers	
list lc_dmdProdDesc if bnfsec=="Sunscreens And Camouflagers" & emo==1
recode emo 1=. if bnfsec=="Sunscreens And Camouflagers" & emo==1	

* The rest!	
list lc_dmdProdDesc if bnfsec=="Skin Fillers And Protectives" & emo==1 // looks ok
list lc_dmdProdDesc if bnfsec=="Other Appliances" & emo==1 // looks OK
list lc_dmdProdDesc if bnfsec=="Oral Nutrition" & emo==1
list lc_dmdProdDesc if bnfsec=="Health Supplements" & emo==1
list lc_dmdProdDesc if bnfsec=="Other Preparations" & emo==1





***************
* 6.1.4 REVIEW BY BNF paragraph
***************
tab bnfpara if emo==1, miss sort f

list lc_dmdProdDesc if bnfpara=="Foods For Special Diets" & emo==1
recode emo 1=. if bnfpara=="Foods For Special Diets" & emo==1




***************
* 6.1.5 SPECIFIC EXCLUSIONS - using code from Harriet's original file
*	- for CPRD data
***************
 * additional specific exclusions (NB: also searches topical corticosteroids)
loc drugsub " "*urea cream*" "*ky jelly*" "*lubricating jelly*" "*canesten*" "
loc drugsub "`drugsub' "*acne*" "*sun *lotion*" "*gentisone*" "*methadone*" "
loc drugsub "`drugsub' "*iodine*" "*sudocream*" "*sential e cream*" "*polytar*" "
loc drugsub "`drugsub' "*benzoyl peroxide*" "*ky jelly*" "*hydroquinone*" "*tretinoin*" "
loc drugsub "`drugsub' "*canesten*" "*dithranol*"  "almond*oil" "*arachis oil*" "
loc drugsub "`drugsub' "*alphosyl*" "*ammonia*"  "*crotamiton*" "*glyceryl trinitrate*" "
loc drugsub "`drugsub' "*glycerol triolea*" "*industrial methylated spirit*" "
loc drugsub "`drugsub' "*glycerin liquid*" "*hand cream*" "*hand balm*" "
loc drugsub "`drugsub' "*hand aquagel*" "*hydrous wool fat*" "*heparin*" "
loc drugsub "`drugsub' "*glycerol sup*" "*glycerol*thymol*" "*glanolin*" "
loc drugsub "`drugsub' "*alpha keri*" "*ichthammol*" "*nutritional*" "*lanette wax*"  "
loc drugsub "`drugsub' "*wool fat solid*" "*liquid paraffin light liquid*" "
loc drugsub "`drugsub' "*hydrocortisone powder*" "*colifoam*" "*suspension*" "
loc drugsub "`drugsub' "*capsules*" "*tablets*" "* otic *" "*gregoderm*" "
loc drugsub "`drugsub' "*ipecacuanha *" "*decubal*" "*pinetarsol*" "*iodinated*" "
loc drugsub "`drugsub' "*thovaline*" "*lubricating jelly*" "*tyrothricin*" "
loc drugsub "`drugsub' "*inhal*" "*quinoderm with hydrocortisone*" "
loc drugsub "`drugsub' "*cough*" "*linctus*" "

loc not_top " "*ear/ey*" "* loz" "* pow" "* dro*" "*nasal*" "*pellets*" "
loc not_top "`not_top' "* eye*" "* tab" "* pel" "* ear" "*cherry*" "*drops*" "
loc not_top "`not_top' "*oint*eye*" "*eye*oint*" "*lipcare*" "*foot*" "*hard*skin*" "
loc not_top "`not_top' "*burns*" "*diabetic*"  "*antiperspirants*" "*eye drops*" "
loc not_top "`not_top' "*injection*" "*oromucosal paste*" "*orabase*" "*intra-articular*" "
loc not_top "`not_top' "*intramuscular*" "*intravitreal*" "
loc not_top "`not_top' "*sunscreen*" "*mouthwash*" "*enema*" "*dental*" "*vaginal*" "*sting*" "
loc not_top "`not_top' "*oral*solution*" "*coolie*" "*heel*" "*nappy*" "*faecal*" "
loc not_top "`not_top' "*stimulant lax*" "*enteral nutrition*" "*stomacare*" "*suppositor*" "
loc not_top "`not_top' "*ulcer*" "*propolis*" "*parasiticidal*" "*bufexamac*" "*moogoo*" "
loc not_top "`not_top' "*sudocrem*" "*lube*" "* inj" "*stoma *" "*urea cycle disorder*" "head lice*" "*protease m*" "
loc not_top "`not_top' "*oromucosal*" "*lubricant*"  "*disk*" "*qvar*" "*inhal*" "*insufflator*" "
loc not_top "`not_top' "*plasters*" "


* identify vars to search
local terms_lc " "lc_mdrProdDesc" "lc_dmdProdDesc" "lc_dmdProdPackDesc" "lc_bnfchap" " 
local terms_lc "`terms_lc' "lc_bnfsec" "lc_bnfpara" "lc_bnfsubpara" "lc_bnfsubst" "lc_bnfprod" "lc_bnfpres" "

foreach wordlist in drugsub not_top {
	foreach word in ``wordlist''{
		foreach term in `terms_lc' {
			display "*** `word' ***"
			list lc_dmdProdDesc bnfchap if strmatch(`term', "`word'") & (emo==1 | top_cs==1)
			recode emo 1=. if strmatch(`term', "`word'")
			recode top_cs 1=. if strmatch(`term', "`word'")	
		} /*end foreach term in `terms_lc'*/	
	} /*end foreach word in ``x''*/
} /*end foreach wordlist in drugsub lip_eye_ear misc inhaler */
 
*Emollients to exclude if not given with top corticosteroid
foreach term in `terms_lc' {
	recode emo 1=. if  strmatch(`term', "*coal tar*") & emo==1 
	recode emo 1=.  if  strmatch(`term', "*calamine*") & emo==1 
	recode emo 1=.  if  strmatch(`term', "*salicylic*") & emo==1 
	recode emo 1=.  if  strmatch(`term', "*vit* e *") & emo==1 
	recode emo 1=.  if  strmatch(`term', "*gentamicin*") & emo==1 
	recode emo 1=.  if  strmatch(`term', "*neomycin*") & emo==1 
	recode emo 1=.  if  strmatch(`term', "*polymyxin*") & emo==1 
	recode emo 1=.  if  strmatch(`term', "*chlorquinaldol*") & emo==1 
	recode emo 1=.  if  strmatch(`term', "*clioquinol*") & emo==1 
	recode emo 1=.  if  strmatch(`term', "*allantoin*") & emo==1 
	recode emo 1=.  if  strmatch(`term', "*terra-cortril*") & emo==1 
	recode emo 1=.  if  strmatch(`term', "*zinc*") & emo==1 
} /*end foreach term in bnfheader productname drugsubstance*/

* drop certain single substances only
drop if lc_bnfsubst=="castor oil"
drop if lc_bnfsubst=="magnesium sulphate"  
drop if lc_bnfsubst=="almond oil"
drop if lc_bnfsubst=="chamomile extract" 
drop if lc_bnfsubst=="chamomile" 
drop if lc_bnfsubst=="zinc oxide"
drop if lc_bnfsubst=="castor oil/zinc oxide" 
drop if lc_bnfsubst=="castor oil/zinc oxide/wool fat" 
drop if lc_bnfsubst=="dimeticone/calamine/zinc oxide"
drop if lc_bnfsubst=="evening primrose oil"
drop if lc_bnfsubst=="coconut oil"
drop if lc_bnfsubst=="fractioned coconut oil"
drop if lc_bnfsubst=="salicylic acid"
drop if lc_bnfsubst=="alpha tocopherol"
 
recode emo 1=.  if  lc_dmdProdDesc=="liquid paraffin"
recode emo 1=.  if  lc_dmdProdDesc=="liquid paraffin 16% lotion"
recode emo 1=.  if  lc_dmdProdDesc=="liquid paraffin emulsion" 
 
 
 
 
***************
* 6.1.6 REVIEW
***************
tab route if emo==1, miss sort f // route of admin
tab bnfsec if emo==1, miss sort f
tab bnfpara if emo==1, miss sort f








/*------------------------------------------------------------------------------
#6.2 Topical steroids
------------------------------------------------------------------------------*/
* first take a look at stuff to identify things that might not be relevant
* first take a look at stuff to identify things that might not be relevant
tab route if top_cs==1, miss sort f // route of admin
tab bnfchap if top_cs==1, sort f	// bnf headers
tab bnfsec if top_cs==1, sort f
tab bnfpara if top_cs==1, sort f
tab bnfsubpara if top_cs==1, sort f
tab bnfsubst if top_cs==1, miss sort f // substances




***************
* 6.2.1 ROUTE
***************
* identify and exclude emolients that are not administered topically
recode top_cs 1=. if (route=="Nasal" | route=="Intramuscular" ///
	| route=="Intravenous" | route=="Ocular" | route=="Auricular" | route=="Inhalation" ///
	| route=="Rectal" | route=="Intralesional" | route=="Intravitreal" ||route=="Oral" ///
	| route=="Subconjunctival")

tab route if top_cs==1, miss sort f // R/V >> looks OK





***************
* 6.2.2 BNF CHAPTER
***************
tab bnfchap if top_cs==1, sort f	// bnf headers

* resp
list lc_dmdProdDesc if bnfchap=="Respiratory System" & top_cs==1
local excterms " "*inhaler*" "*autohaler*" "*accuhaler*" "*nebuliser*" "*nebules*" "*evohaler*" "
local excterms "`excterms' "*twisthaler*" "*turbohaler*" "*respules*" "
local excterms "`excterms' "*clickhaler*" "*rotacaps*" "*cyclocaps*" "*inhalation*" "
foreach word in `excterms' {
	display "*** `word' ***"
	recode top_cs 1=. if strmatch(lc_dmdProdDesc, "`word'") & top_cs==1
} /*end foreach word in `excterms'*/

* ENT
list lc_dmdProdDesc if bnfchap=="Ear, Nose And Oropharynx" & top_cs==1
local excterms " "*nasal*" "*ear*" "*nose*" "*otic*" "
foreach word in `excterms' {
	display "*** `word' ***"
	recode top_cs 1=.  if strmatch(lc_dmdProdDesc, "`word'")  & top_cs==1
} /*end foreach word in `excterms'*/

* GI
list lc_dmdProdDesc if bnfchap=="Gastro-Intestinal System" & top_cs==1
local excterms " "*enema*" "*rectal*" "*supposit*" "*anusol*" "*anugesic*" "*proctosedyl*" "*uniroid*" "
local excterms "`excterms' "*xyloproct*" "*perinal*" "*germoloids*" "*budenofalk*" "*ultraproct*" "
local excterms "`excterms' "*gastro-resistant*" "*cinchocaine*" "*lidocaine*" "
foreach word in `excterms' {
	display "*** `word' ***"
	recode top_cs 1=.  if strmatch(lc_dmdProdDesc, "`word'") & top_cs==1
} /*end foreach word in `excterms'*/



* Endocrine
list lc_dmdProdDesc if bnfchap=="Endocrine System" & top_cs==1
recode top_cs 1=.  if strmatch(lc_dmdProdDesc, "*injection*") & top_cs==1

* Eye
list lc_dmdProdDesc if bnfchap=="Eye" & top_cs==1
recode top_cs 1=.  if bnfchap=="Eye" & top_cs==1
recode top_cs 1=.  if strmatch(lc_dmdProdDesc, "*eye*") & top_cs==1

* MSk
list lc_dmdProdDesc if bnfchap=="Musculoskeletal & Joint Diseases" & top_cs==1
recode top_cs 1=.  if bnfchap=="Musculoskeletal & Joint Diseases" & top_cs==1

* Other drugs
list lc_dmdProdDesc if bnfchap=="Other Drugs And Preparations" & top_cs==1 // all powder for injection >> drop
recode top_cs 1=.  if bnfchap=="Other Drugs And Preparations" & top_cs==1

* Anaesthetics
list lc_dmdProdDesc if bnfchap=="Anaesthesia" & top_cs==1 // all mouthwash
recode top_cs 1=.  if bnfchap=="Anaesthesia" & top_cs==1 


* R/V
tab bnfchap if top_cs==1, miss sort f	// bnf headers






***************
* 6.2.3 BNF sections
***************
tab bnfsec if top_cs==1, sort f
/*

                            BNF Section |      Freq.     Percent        Cum.
----------------------------------------+-----------------------------------
                Topical Corticosteroids |      1,658       96.34       96.34
  Preparations For Eczema And Psoriasis |         55        3.20       99.54
            Corticosteroids (Endocrine) |          4        0.23       99.77
 Top Local Anaesthetics & Antipruritics |          4        0.23      100.00
----------------------------------------+-----------------------------------
                                  Total |      1,721      100.00

*/


list lc_dmdProdDesc if bnfsec=="Corticosteroids (Endocrine)" & top_cs==1
list lc_dmdProdDesc if bnfsec=="Top Local Anaesthetics & Antipruritics" & top_cs==1


tab bnfpara if top_cs==1, sort f
tab bnfsubpara if top_cs==1, sort f

tab bnfsubst if top_cs==1, miss sort f // substances
	// NB: calcipotriol with topical CS more likely to be used in psoriasis
	// what about menthol? is this really used in eczema?


recode top_cs 1=. if strmatch(lc_dmdProdDesc, "*powder*") & top_cs==1



**********
* UPDATED FROM HERE
********************


***************
* 6.2.4 IDENTFY POTENCY of topical CS
***************
* Flag topical steroids into mild, moderate, potent - originally based on BNF headers
* but this dataset does not contain mild/moderate/severe BNF chapter headers
gen potency=.
lab def potency 1 mild 2 moderate 3 potent
lab val potency potency
label var potency "strength of topical corticosteroid"



***** BROAD SEARCHES
* MILD potency
recode potency .=1 if strmatch(lc_dmdProdDesc, "*hydrocortisone acet. 1%*") & top_cs==1
recode potency .=1 if strmatch(lc_dmdProdDesc, "*hydrocortisone 1 %*") & top_cs==1
recode potency .=1 if strmatch(lc_dmdProdDesc, "*hydrocortisone 1%*") & top_cs==1
recode potency .=1 if strmatch(lc_dmdProdDesc, "*hydrocortisone 0.5%*") & top_cs==1
recode potency .=1 if strmatch(lc_dmdProdDesc, "*hydrocortisone 0.5 %*") & top_cs==1
recode potency .=1 if strmatch(lc_dmdProdDesc, "*hydrocortisone .5 %*") & top_cs==1
recode potency .=1 if strmatch(lc_dmdProdDesc, "*hydrocortisone acetate 1%*") & top_cs==1
recode potency .=1 if strmatch(lc_dmdProdDesc, "*calmurid hc*") & top_cs==1
recode potency .=1 if strmatch(lc_dmdProdDesc, "*efcortelan*") & top_cs==1
recode potency .=1 if strmatch(lc_dmdProdDesc, "*vioform-hydrocortisone*") & top_cs==1
recode potency .=1 if strmatch(lc_dmdProdDesc, "*hydromol hc*") & top_cs==1
recode potency .=1 if strmatch(lc_dmdProdDesc, "*eurax hydrocortisone*") & top_cs==1
recode potency .=1 if strmatch(lc_dmdProdDesc, "*eurax hc*") & top_cs==1
recode potency .=1 if strmatch(lc_dmdProdDesc, "*quinocort*") & top_cs==1
recode potency .=1 if strmatch(lc_dmdProdDesc, "*0.5% hydrocortisone*") & top_cs==1
recode potency .=1 if bnfsubst=="Hydrocortisone Acetate" & top_cs==1
recode potency .=1 if strmatch(lc_dmdProdDesc, "*hydrocortisone 0.1%*") & top_cs==1
recode potency .=1 if strmatch(lc_dmdProdDesc, "*nystaform hc*") & top_cs==1
recode potency .=1 if strmatch(lc_dmdProdDesc, "*nystaform-hc*") & top_cs==1
recode potency .=1 if strmatch(lc_dmdProdDesc, "*timodine*") & top_cs==1
recode potency .=1 if strmatch(lc_dmdProdDesc, "*terra-cortril*") & top_cs==1
recode potency .=1 if strmatch(lc_dmdProdDesc, "*fucidin h*") & top_cs==1
recode potency .=1 if strmatch(lc_dmdProdDesc, "*canesten hc*") & top_cs==1 
recode potency .=1 if strmatch(lc_dmdProdDesc, "*daktacort*") & top_cs==1 
recode potency .=1 if strmatch(lc_dmdProdDesc, "*econacort*") & top_cs==1
recode potency .=1 if strmatch(lc_dmdProdDesc, "*dioderm*") & top_cs==1 
recode potency .=1 if strmatch(lc_dmdProdDesc, "*mildison*") & top_cs==1 
recode potency .=1 if strmatch(lc_dmdProdDesc, "*synalar 1 in 10*") & top_cs==1
recode potency .=1 if strmatch(lc_dmdProdDesc, "*hydrocort*1*%*") & top_cs==1
recode potency .=1 if strmatch(lc_dmdProdDesc, "*hydrocort*2*%*") & top_cs==1
recode potency .=1 if strmatch(lc_dmdProdDesc, "*dermacort*") & top_cs==1
recode potency .=1 if strmatch(lc_dmdProdDesc, "*hc45*") & top_cs==1
recode potency .=1 if strmatch(lc_dmdProdDesc, "*zenoxone*") & top_cs==1
recode potency .=1 if strmatch(lc_dmdProdDesc, "*fluocinolone acetonide 0.0025%*") & top_cs==1
recode potency .=1 if bnfsubst=="Fluocinolone Acetonide" & strmatch(lc_dmdProdDesc, "*0.0025%*") & top_cs==1


* MODERATE potency
recode potency .=2 if strmatch(lc_dmdProdDesc, "*synalar 1 in 4*") & top_cs==1
recode potency .=2 if strmatch(lc_dmdProdDesc, "*trimovate*") & top_cs==1
recode potency .=2 if strmatch(lc_dmdProdDesc, "*betnovate-rd*") & top_cs==1
recode potency .=2 if strmatch(lc_dmdProdDesc, "*betnovate rd*") & top_cs==1
recode potency .=2 if strmatch(lc_dmdProdDesc, "*eumovate*") & top_cs==1
recode potency .=2 if strmatch(lc_dmdProdDesc, "*haelan*") & top_cs==1 
recode potency .=2 if strmatch(lc_dmdProdDesc, "*modrasone*") & top_cs==1 
recode potency .=2 if strmatch(lc_dmdProdDesc, "*alphaderm*") & top_cs==1
recode potency .=2 if strmatch(lc_dmdProdDesc, "*clobavate*") & top_cs==1
recode potency .=2 if strmatch(lc_dmdProdDesc, "*clobetasone butyrate 0.05%*") & top_cs==1
recode potency .=2 if strmatch(lc_dmdProdDesc, "*fludroxycortide 0.0125%*") & top_cs==1
recode potency .=2 if strmatch(lc_dmdProdDesc, "*betamethasone valerate 0.025%*") & top_cs==1
recode potency .=2 if strmatch(lc_dmdProdDesc, "*fluocinolone acetonide 0.00625%*") & top_cs==1
recode potency .=2 if bnfsubst=="Clobetasone Butyrate" & top_cs==1
recode potency .=2 if bnfsubst=="Fluocinolone Acetonide" & strmatch(lc_dmdProdDesc, "*0.00625%*") & top_cs==1
recode potency .=2 if bnfsubst=="Betamethasone Valerate" & strmatch(lc_dmdProdDesc, "*0.025%*") & top_cs==1
recode potency .=2 if bnfsubst=="Fludroxycortide" & top_cs==1
recode potency .=2 if bnfsubst=="Alclometasone Dipropionate" & top_cs==1
recode potency .=2 if bnfsubst=="Triamcinolone Acetonide" & top_cs==1
recode potency .=2 if bnfsubst=="Desoximetasone" & strmatch(lc_dmdProdDesc, "*0.05%*") & top_cs==1
recode potency .=2 if strmatch(lc_dmdProdDesc, "*fluprednidene*") & top_cs==1
recode potency .=2 if strmatch(lc_dmdProdDesc, "*fluprenidine*") & top_cs==1 //likely a typo of fluprednidene
recode potency .=2 if strmatch(lc_dmdProdDesc, "*acorvio*") & top_cs==1



* POTENT
recode potency .=3 if strmatch(lc_dmdProdDesc, "*ultralanum plain*") & top_cs==1
recode potency .=3 if strmatch(lc_dmdProdDesc, "*betamethasone valerate 0.1%*") & top_cs==1
recode potency .=3 if strmatch(lc_dmdProdDesc, "*betamethasone*") & top_cs==1
recode potency .=3 if strmatch(lc_dmdProdDesc, "*aureocort*") & top_cs==1
recode potency .=3 if strmatch(lc_dmdProdDesc, "*fucibet*") & top_cs==1
recode potency .=3 if strmatch(lc_dmdProdDesc, "*lotriderm*") & top_cs==1
recode potency .=3 if strmatch(lc_dmdProdDesc, "*synalar c*") & top_cs==1
recode potency .=3 if strmatch(lc_dmdProdDesc, "*synalar n*") & top_cs==1
recode potency .=3 if strmatch(lc_dmdProdDesc, "*betnovate*") & top_cs==1
recode potency .=3 if strmatch(lc_dmdProdDesc, "*dermovate*") & top_cs==1
recode potency .=3 if strmatch(lc_dmdProdDesc, "*diprosalic*") & top_cs==1
recode potency .=3 if strmatch(lc_dmdProdDesc, "*locoid*") & top_cs==1
recode potency .=3 if strmatch(lc_dmdProdDesc, "*betacap*") & top_cs==1
recode potency .=3 if strmatch(lc_dmdProdDesc, "*fluocortolone hexanoate 0.25%*") & top_cs==1
recode potency .=3 if strmatch(lc_dmdProdDesc, "*beclometasone dipropionate 0.025%*") & top_cs==1
recode potency .=3 if strmatch(lc_dmdProdDesc, "*fluticasone propionate 0.005%*") & top_cs==1
recode potency .=3 if strmatch(lc_dmdProdDesc, "*fluticasone propionate 0.05%*") & top_cs==1
recode potency .=3 if strmatch(lc_dmdProdDesc, "*betamethasone dipropionate 0.05%*") & top_cs==1
recode potency .=3 if strmatch(lc_dmdProdDesc, "*cutivate*") & top_cs==1
recode potency .=3 if strmatch(lc_dmdProdDesc, "*diprosone*") & top_cs==1
recode potency .=3 if strmatch(lc_dmdProdDesc, "*elocon*") & top_cs==1
recode potency .=3 if strmatch(lc_dmdProdDesc, "*metosyn*") & top_cs==1
recode potency .=3 if strmatch(lc_dmdProdDesc, "*nerisone*") & top_cs==1
recode potency .=3 if strmatch(lc_dmdProdDesc, "*clarelux*") & top_cs==1
recode potency .=3 if strmatch(lc_dmdProdDesc, "*estrivex*") & top_cs==1
recode potency .=3 if strmatch(lc_dmdProdDesc, "*clobetasol propionate 0.05%*") & top_cs==1
recode potency .=3 if bnfsubst=="Mometasone Furoate" & top_cs==1
recode potency .=3 if bnfsubst=="Clobetasol Propionate" & top_cs==1
recode potency .=3 if bnfsubst=="Beclometasone Dipropionate" & top_cs==1
recode potency .=3 if bnfsubst=="Fluocinonide" & top_cs==1
recode potency .=3 if bnfsubst=="Fluocinolone Acetonide" & strmatch(lc_dmdProdDesc, "*0.025%*") & top_cs==1
recode potency .=3 if strmatch(lc_dmdProdDesc, "*fluocinolone acetonide 0.025%*") & top_cs==1
recode potency .=3 if bnfsubst=="Betamethasone Valerate" & strmatch(lc_dmdProdDesc, "*0.1%*") & top_cs==1
recode potency .=3 if bnfsubst=="Betamethasone Valerate" & strmatch(lc_dmdProdDesc, "*0.12%*") & top_cs==1
recode potency .=3 if bnfsubst=="Fluticasone Propionate (Top)" & top_cs==1
recode potency .=3 if bnfsubst=="Diflucortolone Valerate" & top_cs==1
recode potency .=3 if strmatch(lc_dmdProdDesc, "*methylprednisolone aceponate 0.1%*") & top_cs==1
recode potency .=3 if strmatch(lc_dmdProdDesc, "*advantan*") & top_cs==1
recode potency .=3 if bnfsubst=="Betamethasone Valerate" & top_cs==1
recode potency .=3 if bnfsubst=="Desoximetasone" & strmatch(lc_dmdProdDesc, "*0.25%*") & top_cs==1
recode potency .=3 if strmatch(lc_dmdProdDesc, "*bettamousse*") & top_cs==1
recode potency .=3 if strmatch(lc_dmdProdDesc, "*etrivex*") & top_cs==1
recode potency .=3 if strmatch(lc_dmdProdDesc, "*clobetasol*") & top_cs==1
recode potency .=3 if strmatch(lc_dmdProdDesc, "*halcinonide*") & top_cs==1
recode potency .=3 if strmatch(lc_dmdProdDesc, "*halciderm*") & top_cs==1
recode potency .=3 if strmatch(lc_dmdProdDesc, "*stiedex*") & top_cs==1


* R/V products with missing potency variable
tab potency if top_cs==1, miss
tab bnfsubst if top_cs==1 & potency==., miss sort f // substances
tab lc_dmdProdDesc if top_cs==1 & potency==., miss sort f // products




/*------------------------------------------------------------------------------
#6.3 Oral steroids
------------------------------------------------------------------------------*/
* first take a look at stuff to identify things that might not be relevant
tab route if oral_gc==1, miss sort f // route of admin
tab bnfchap if oral_gc==1, sort f	// bnf headers
tab bnfsec if oral_gc==1, sort f
tab bnfpara if oral_gc==1, sort f
tab bnfsubpara if oral_gc==1, sort f
tab bnfsubst if oral_gc==1, miss sort f // substances


***************
*6.3.1 ROUTE
***************
* identify and exclude oral gc that are not administered orally
recode oral_gc 1=. if (route=="Oromucosal")
recode oral_gc 1=. if (route=="Intravitreal")


tab route if oral_gc==1, miss sort f // R/V >> looks OK

* by text in product name (terms that definitely need to be excluded)
list lc_dmdProdDesc if oral_gc==1
local excterms " "*injection*" "*suspension*" "*spray*" "*cream*" "*cutaneous*" "*vials*" "*enema*" "*powder*" "*ointment*" "
foreach word in `excterms' {
	display "*** `word' ***"
	recode oral_gc 1=. if strmatch(lc_dmdProdDesc, "`word'") & oral_gc==1
} /*end foreach word in `excterms'*/




***************
*6.3.2 REVIEW BY HIGH-LEVEL BNF CHAPTER
***************
* identify and exclude relevant medications
tab bnfchap if oral_gc==1, sort f	// bnf headers

/*
                            BNF Chapter |      Freq.     Percent        Cum.
----------------------------------------+-----------------------------------
                       Endocrine System |      1,327       39.13       39.13
                                   Skin |      1,110       32.73       71.87
                     Respiratory System |        361       10.65       82.51
                                    Eye |        197        5.81       88.32
               Ear, Nose And Oropharynx |        181        5.34       93.66
               Gastro-Intestinal System |        142        4.19       97.85
       Musculoskeletal & Joint Diseases |         52        1.53       99.38
           Other Drugs And Preparations |         13        0.38       99.76
                            Anaesthesia |          8        0.24      100.00
----------------------------------------+-----------------------------------
                                  Total |      3,391      100.00

*/


* R/V drugs listed under chapters other than 'Skin'
* Endocrine system
list lc_dmdProdDesc if bnfchap=="Endocrine System" & oral_gc==1
local excterms " "*injection*" "
foreach word in `excterms' {
	display "*** `word' ***"
	recode oral_gc 1=. if strmatch(lc_dmdProdDesc, "`word'") & bnfchap=="Endocrine System" & oral_gc==1
} /*end foreach word in `excterms'*/

* Respiratory System
list lc_dmdProdDesc if bnfchap=="Respiratory System" & oral_gc==1
local excterms " "*dry powder*" "*inhal*" "*haler*" "*nebuliser*" " *nebulizer*" "*disks*" "*cyclocaps*" "*rotacaps*" "*respules*" "
foreach word in `excterms' {
	display "*** `word' ***"
	recode oral_gc 1=. if strmatch(lc_dmdProdDesc, "`word'") & bnfchap=="Respiratory System" & oral_gc==1
} /*end foreach word in `excterms'*/

* Eye
list lc_dmdProdDesc if bnfchap=="Eye" & oral_gc==1
local excterms " "*drops*" "*eye ointment*" "
foreach word in `excterms' {
	display "*** `word' ***"
	recode oral_gc 1=. if strmatch(lc_dmdProdDesc, "`word'") & bnfchap=="Eye" & oral_gc==1
} /*end foreach word in `excterms'*/

* Ear, Nose And Oropharynx
list lc_dmdProdDesc if bnfchap=="Ear, Nose And Oropharynx" & oral_gc==1
local excterms " "*oromucosal paste*" "*nasal spray*" "*drops*" "*ointment*" "*spray*" "*buccal*" "*pastilles*" "*pellets*" "
foreach word in `excterms' {
	display "*** `word' ***"
	recode oral_gc 1=. if strmatch(lc_dmdProdDesc, "`word'") & bnfchap=="Ear, Nose And Oropharynx" & oral_gc==1
} /*end foreach word in `excterms'*/

* Gastro-Intestinal System
list lc_dmdProdDesc if bnfchap=="Gastro-Intestinal System" & oral_gc==1
local excterms " "*suppositor*" "*ointment*" "*enema*" "*cream*" "*rectal*" "*spray*" "*aerosol*" "
foreach word in `excterms' {
	display "*** `word' ***"
	recode oral_gc 1=. if strmatch(lc_dmdProdDesc, "`word'") & bnfchap=="Gastro-Intestinal System" & oral_gc==1
} /*end foreach word in `excterms'*/

* Musculoskeletal & Joint Diseases
list lc_dmdProdDesc if bnfchap=="Musculoskeletal & Joint Diseases" & oral_gc==1
local excterms " "*injection*" "
foreach word in `excterms' {
	display "*** `word' ***"
	recode oral_gc 1=. if strmatch(lc_dmdProdDesc, "`word'") & bnfchap=="Musculoskeletal & Joint Diseases" & oral_gc==1
} /*end foreach word in `excterms'*/

* Other Drugs And Preparations
list lc_dmdProdDesc if bnfchap=="Other Drugs And Preparations" & oral_gc==1
local excterms " "*powder*" "
foreach word in `excterms' {
	display "*** `word' ***"
	recode oral_gc 1=. if strmatch(lc_dmdProdDesc, "`word'") & bnfchap=="Other Drugs And Preparations" & oral_gc==1
} /*end foreach word in `excterms'*/

* Anaesthesia
list lc_dmdProdDesc if bnfchap=="Anaesthesia" & oral_gc==1
local excterms " "*mouthwash*" "
foreach word in `excterms' {
	display "*** `word' ***"
	recode oral_gc 1=. if strmatch(lc_dmdProdDesc, "`word'") & bnfchap=="Anaesthesia" & oral_gc==1
} /*end foreach word in `excterms'*/

* R/V
tab bnfchap if oral_gc==1, sort miss f	// bnf headers

***************
*6.3.3  REVIEW BY BNF CHAPTER SECTION
***************
tab bnfsec if oral_gc==1, sort f

* topical CS
list lc_dmdProdDesc if bnfsec=="Topical Corticosteroids" & oral_gc==1
recode oral_gc 1=. if bnfsec=="Topical Corticosteroids" & oral_gc==1
	// remove all
 
* Preparations For Eczema And Psoriasis 
list lc_dmdProdDesc if bnfsec=="Preparations For Eczema And Psoriasis" & oral_gc==1
local excterms ""*ointment*" "* gel*" "*cream*" "
foreach word in `excterms' {
	display "*** `word' ***"
	recode oral_gc 1=. if strmatch(lc_dmdProdDesc, "`word'") & bnfsec=="Preparations For Eczema And Psoriasis" & oral_gc==1
} /*end foreach word in `excterms'*/

* Chronic Bowel Disorders
list lc_dmdProdDesc if bnfsec=="Chronic Bowel Disorders" & oral_gc==1
	// keep???

*  Miscellaneous Topical Preparations
list lc_dmdProdDesc if bnfsec=="Miscellaneous Topical Preparations" & oral_gc==1
	// drop all
recode oral_gc 1=. if bnfsec=="Miscellaneous Topical Preparations" & oral_gc==1

* Acne and Rosacea
list lc_dmdProdDesc if bnfsec=="Acne and Rosacea" & oral_gc==1
	// contain clindamycin >> drop
	recode oral_gc 1=. if bnfsec=="Acne and Rosacea" & oral_gc==1
	
* Top Local Anaesthetics & Antipruritics 
list lc_dmdProdDesc if bnfsec=="Top Local Anaesthetics & Antipruritics" & oral_gc==1
	//drop
	recode oral_gc 1=. if bnfsec=="Top Local Anaesthetics & Antipruritics" & oral_gc==1
	
* Preparations For Eczema And Psoriasis
list lc_dmdProdDesc if bnfsec=="Preparations For Eczema And Psoriasis" & oral_gc==1
	//keep



***************
*6.3.4 REVIEW BY BNF paragraph
***************
tab bnfpara if oral_gc==1, miss sort f

* ""
list lc_dmdProdDesc if bnfpara=="" & oral_gc==1
local excterms " "*turbohaler*" "
foreach word in `excterms' {
	display "*** `word' ***"
	recode oral_gc 1=. if strmatch(lc_dmdProdDesc, "`word'") & bnfpara=="" & oral_gc==1
} /*end foreach word in `excterms'*/

* Replacement Therapy
list lc_dmdProdDesc if bnfpara=="Replacement Therapy" & oral_gc==1
// These are all Fludrocortisone (corticosteroid for adrenal insufficiency)
recode oral_gc 1=. if bnfpara=="Replacement Therapy" & oral_gc==1

*  Preparations For Psoriasis
list lc_dmdProdDesc if bnfpara=="Preparations For Psoriasis" & oral_gc==1
recode oral_gc 1=. if bnfpara=="Preparations For Psoriasis" & oral_gc==1
 
 
***************
*6.3.5 REVIEW
***************
tab route if oral_gc==1, miss sort f // route of admin
tab bnfsec if oral_gc==1, miss sort f
tab bnfpara if oral_gc==1, miss sort f




*Do we need to identify systemic cortciosteroids???



/*------------------------------------------------------------------------------
#6.4 Topical calcineurin inhibitors
------------------------------------------------------------------------------*/
* first take a look at stuff to identify things that might not be relevant
tab route if tcal==1, miss sort f // route of admin
tab bnfchap if tcal==1, sort f	// bnf headers
tab bnfsec if tcal==1, sort f
tab bnfpara if tcal==1, sort f
tab bnfsubpara if tcal==1, sort f
tab bnfsubst if tcal==1, miss sort f // substances


***************
*6.4.1  ROUTE
***************
* identify and exclude oral gc that are not administered orally
recode tcal 1=. if (route=="Oral")
recode tcal 1=. if (route=="Oromucosal")
recode tcal 1=. if (route=="Intravenous")
recode tcal 1=. if (route=="Rectal")


tab route if tcal==1, miss sort f // R/V >> looks OK

* by text in product name (only terms that definitely need to be excluded)
list lc_dmdProdDesc if tcal==1
local excterms " "*suppositories*" "*mouthwash*" "*granules*" "*sachets*" "*infusion*" "
foreach word in `excterms' {
	display "*** `word' ***"
	recode tcal 1=. if strmatch(lc_dmdProdDesc, "`word'") & tcal==1
} /*end foreach word in `excterms'*/

*Looks good


/*------------------------------------------------------------------------------
#6.5 Systemic drugs
------------------------------------------------------------------------------*/
* first take a look at stuff to identify things that might not be relevant
tab route if sys==1, miss sort f // route of admin
tab bnfchap if sys==1, sort f	// bnf headers
tab bnfsec if sys==1, sort f
tab bnfpara if sys==1, sort f
tab bnfsubpara if sys==1, sort f
tab bnfsubst if sys==1, miss sort f // substances


***************
*6.5.1 ROUTE
***************
* identify and exclude oral gc that are not administered orally
recode sys 1=. if (route=="Ocular")
recode sys 1=. if (route=="Cutaneous")
recode sys 1=. if (route=="Intraventricular cardiac")


tab route if sys==1, miss sort f // R/V >> looks OK

* by text in product name (only terms that definitely need to be excluded)
list lc_dmdProdDesc if sys==1
local excterms " "*eye drops*" "*eye ointment*" "
foreach word in `excterms' {
	display "*** `word' ***"
	recode sys 1=. if strmatch(lc_dmdProdDesc, "`word'") & sys==1
} /*end foreach word in `excterms'*/

*looks good



/*------------------------------------------------------------------------------
#6.6 flag injections
------------------------------------------------------------------------------*/ 
* identify records where bnfheader, product or drug substance
* include inj or injection
gen injection=0 if oral_gc==1
loc inj1 " "* inj *" "*injection*" "* inj" " 

foreach term in lc_dmdProdDesc lc_bnfsubst {
	display "*** `term' ***"
	foreach word in `inj1' {
		display "*** `word' ***"
		list lc_dmdProdDesc  if  strmatch(`term', "`word'") & inj==0
		recode  inj 0=1 if strmatch(`term', "`word'")
	} /*foreach word in ``x''*/ 
} /*end foreach term in bnfheader productname drugsubstance*/

recode oral_gc 1=0 if injection==1 // edit oral_cs variable if drug is an injection




/*------------------------------------------------------------------------------
#6.7 Other immunosuppressants
	Exclude Myco, aza and cic given by infusion or in the eye in eczema
------------------------------------------------------------------------------*/ 
* identify mycophenolate, azathioprine or ciclosporin given by injection/infusion
* or in the eye
local badroute " "*infusion*" "*injection*" "* eye *" "
foreach x in `badroute' {
	display "****************** `x' ******************"
		display "*** `var' ***"
		list lc_dmdProdDesc if strmatch(lc_dmdProdDesc, "`x'") & (sys==1)
		recode sys 1=. if strmatch(lc_dmdProdDesc, "`x'")  
} /*end foreach x in `badroute'*/
	



	
/*------------------------------------------------------------------------------
#6.8 review additional bnf headers and drop any that are not relevant
------------------------------------------------------------------------------*/ 
* review
tab lc_bnfsec if keep==1 & tot==., sort freq
tab lc_bnfsec if keep==. & tot==1, sort freq

drop if lc_bnfsec=="sunscreens and camouflagers"
drop if lc_bnfsec=="anti-infective skin preparations"
drop if lc_bnfsec=="skin cleansers,antiseptics & desloughing"
drop  if lc_bnfsec=="acne and rosacea"
drop  if lc_bnfsec=="shampoo&other preps for scalp&hair cond"
drop  if lc_bnfsec=="top local anaesthetics & antipruritics"
drop  if lc_bnfsec=="preparations for warts and calluses"
drop  if lc_bnfsec=="topical circulatory preparations"
drop  if lc_bnfsec=="wound management products"
drop  if lc_bnfsec=="antiperspirants"

/*------------------------------------------------------------------------------
#6.9 review additional bnf sub-headers and drop any that are not relevant
------------------------------------------------------------------------------*/ 
tab lc_bnfpara  if tot==.
drop  if lc_bnfpara=="preparations for psoriasis"

tab lc_bnfprod  if lc_bnfpara=="antimetabolites" & tot==.
* Looks goood; these shouldn't be included

tab lc_bnfprod  if lc_bnfpara=="antiproliferative immunosuppressants" & tot==.
* Looks goood; these shouldn't be included

tab lc_bnfprod  if lc_bnfpara=="cortico'oids & other immunosuppressants" & tot==.
* Looks goood; these shouldn't be included

tab lc_bnfprod  if lc_bnfpara=="drugs affecting the immune response" & tot==.
* Looks goood; these shouldn't be included

tab lc_bnfprod  if lc_bnfpara=="management of skin conditions" & tot==.
* Looks goood; these shouldn't be included

tab lc_bnfprod  if lc_bnfpara=="miscellaneous topical preparations" & tot==.
* Looks goood; these shouldn't be included

tab lc_bnfprod  if lc_bnfpara=="preparations for eczema" & tot==.
* Looks goood; these shouldn't be included

tab lc_bnfprod  if lc_bnfpara=="topical corticosteroids" & tot==.
* I think these should be inlcuded
recode top_cs .=1 if lc_bnfpara=="topical corticosteroids" & tot==.



/*******************************************************************************
#7. Only keep relevant products
*******************************************************************************/
drop total		  
gen total=.
foreach x in emo top_cs tcal oral_gc sys {
	recode total .=1 if `x'==1
} /*end foreach x in emo top_cs tcal oral_cs aza metho cic myco*/

keep if total==1

/*******************************************************************************
#8. Tidy up, save and close
*******************************************************************************/

sort lc_bnfchap
compress

label data "SNOMED codes for eczema therapy Jan 2020 build"
notes: SNOMED codes for eczema therapy Jan 2020 build
notes: ${filename} / TS

save ${pathCodelists}/SNOMEDcodes-eczemaRx, replace






log close
