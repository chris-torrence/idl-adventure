;--------------------------------------------------------------------
; MODIFIED BY KENT BLACKETT
;             ENGINEERING SYSTEMS GROUP
;             DIGITAL EQUIPMENT CORP.
;             15-JUL-77
; MODIFIED BY   BOB SUPNIK
;               DISK ENGINEERING
;               21-OCT-77
; MODIFIED BY   BOB SUPNIK
;               DISK ENGINEERING
;               25-AUG-78
; MODIFIED BY   BOB SUPNIK
;               SMALL SYSTEMS
;               12-NOV-78
; ORIGINAL VERSION WAS FOR DECSYSTEM-10
; NEXT VERSION WAS FOR FORTRAN IV-PLUS UNDER
; THE IAS OPERATING SYSTEM ON THE PDP-11/70
; THIS VERSION IS FOR FORTRAN IV (V01C OR LATER)
; UNDER RT-11 ON *ANY* PDP-11

; Modified March 1992 by Ken Plotkin, Wyle Labs (former DECUS member)
; to run under MS-DOS, using Microsoft Fortran.  Changes are annotated
; using upper/lower case text.


;  CURRENT LIMITS:
;       750 TRAVEL OPTIONS (TRAVEL, TRVSIZ).
;       300 VOCABULARY WORDS (KTAB, ATAB, TABSIZ).
;       150 LOCATIONS (LTEXT, STEXT, KEY, CONDITIONS, ABB, ATLOC, LOCSIZ).
;       100 OBJECTS (PLAC, PLACE, FIXD, FIXED, LINK [TWICE], PTEXT, PROP).
;        35 "ACTION" VERBS (ACTSPK, VRBSIZ).
;       205 RANDOM MESSAGES (RTEXT, RTXSIZ).
;        12 DIFFERENT PLAYER CLASSIFICATIONS (CTEXT, CVAL, CLSMAX).
;        20 HINTS, LESS 3 (HINTLC, HINTED, HINTS, HNTSIZ).
;  THERE ARE ALSO LIMITS WHICH CANNOT BE EXCEEDED DUE TO THE STRUCTURE OF
;  THE DATABASE.  (E.G., THE VOCABULARY USES N/1000 TO DETERMINE WORD TYPE,
;  SO THERE CAN'T BE MORE THAN 1000 WORDS.)  THESE UPPER LIMITS ARE:
;       1000 NON-SYNONYMOUS VOCABULARY WORDS
;       300 LOCATIONS
;       100 OBJECTS

;--------------------------------------------------------------------
pro IDL_Adventure_Common
  compile_opt idl2, hidden
  common VERSN, VMAJ,VMIN,VEDIT, TRUE, FALSE, DEBUG
  common TXTCOM, RTEXT,LINES,ASCVAR,WIDBASE,WIDTEXT,WIDINPUT, $
    COMMAND,CMD,NLINES
  common ALPHAS, BLANK,EOF,CYE,CY,CNO,CN,CPO,CUR,CWE,CST, $
    CEN,CTE
  common VOCCOM, KTAB,ATAB,A2TAB,TABSIZ
  common PLACOM, ATLOC,LINK,PLACE,FIXED,HOLDNG
  common PTXCOM, PTEXT
  common ABBCOM, ABB
  common MISCOM, LINUSE,TRVS,CLSSES,OLDLOC,OLDLC2,LOC,CVAL,TK,NEWLOC, $
    KEY,PLAC,FIXD,ACTSPK,CONDITIONS,HINTS,HNTMAX,PROP,TALLY,TALLY2, $
    HINTLC,CHLOC,CHLOC2,DSEEN,DFLAG,DLOC,DALTLC,KEYS,LAMP,GRATE, $
    CAGE,ROD,ROD2,STEPS,BIRD,DOOR,PILLOW,SNAKE,FISSUR,TABLET, $
    CLAM,OYSTER,MAGZIN,DWARF,KNIFE,FOOD,BOTTLE,WATER,OIL,PLANT, $
    PLANT2,AXE,MIRROR,DRAGON,CHASM,TROLL,TROLL2,BEAR,MESSAG,VEND, $
    BATTER,NUGGET,COINS,CHEST,EGGS,TRIDNT,VASE,EMRALD,PYRAM, $
    PEARL,RUG,CHAIN,BACK,LOOK,CAVE,NULL,ENTRNC,DPRSSN,SAY,LOCK, $
    THROW,FIND,INVENT,TURNS,LMWARN,KNFLOC,DETAIL,ABBNUM, $
    NUMDIE,MAXDIE,DKILL,FOOBAR,BONUS,CLOCK1,CLOCK2,LIMIT, $
    CLOSNG,PANIC,CLOSED,GAVEUP,SCORNG,ODLOC,STREAM,SPICES,HINT
  common MISC2, RTXSIZ,CLSMAX,MAGSIZ,LOCSIZ,CTEXT,STEXT,LTEXT, $
    SECT,TRAVEL,TRVCON,TRVLOC,TRVSIZ,TABNDX,OBJ,J,K,VERB,HNTSIZ, $
    MAXTRS,HINTED,HNTLOC,KK,WZDARK
end

;--------------------------------------------------------------------
function IDL_Adventure_Main, currCmd

  compile_opt idl2, hidden

  common VERSN
  common TXTCOM
  common ALPHAS
  common VOCCOM
  common PLACOM
  common PTXCOM
  common ABBCOM
  common MISCOM
  common MISC2

  if (N_Elements(currCmd) eq 1) then begin
    case (currCmd) of
    -1: ; start over
    1: goto, CMD1
    2: goto, CMD2
    3: goto, CMD3
    4: goto, CMD4
    5: goto, CMD5
    6: goto, CMD6
    7: goto, CMD7
    8: goto, CMD8
    9: goto, CMD9
    else: Message, 'Unknown command number'
    endcase
  endif

;L2605
  _RSPEAK_, 202

;L1:     I = RND[1]
  _RSPEAK_, 65
  if (~DEBUG && WIDBASE gt 0) then return, 3
CMD3:
  HINTED[3] = _YES_(65,1,0)
  NEWLOC = 1
  LOC = NEWLOC
  OLDLOC = LOC
  LIMIT = 330 + debug*10000
  if (HINTED[3]) then LIMIT = 1000 + debug*10000

;  CAN'T LEAVE CAVE ONCE IT'S CLOSING (EXCEPT BY MAIN OFFICE).

L2:
  if (NEWLOC ge 9 || NEWLOC eq 0 || ~CLOSNG) then goto, L71
      _RSPEAK_, 130
      NEWLOC = LOC
      if (~PANIC) then CLOCK2 = 15
      PANIC = TRUE

;  SEE IF A DWARF HAS SEEN HIM AND HAS COME FROM WHERE HE WANTS TO GO.  IF SO,
;  THE DWARF'S BLOCKING HIS WAY.  IF COMING FROM PLACE FORBIDDEN TO PIRATE
;  (DWARVES ROOTED IN PLACE) LET HIM GET OUT (AND ATTACKED).

L71:
  if (NEWLOC eq LOC || _Forced_(LOC) || _Bitset_(loc,3)) then goto, L74
  for I = 1,5 do begin
    if (ODLOC[I] ne NEWLOC || ~DSEEN[I]) then continue
    NEWLOC = LOC
    _RSPEAK_, 2
    break
  endfor

L74:    LOC = NEWLOC

;  DWARF STUFF.  SEE EARLIER COMMENTS FOR DESCRIPTION OF VARIABLES.  REMEMBER
;  SIXTH DWARF IS PIRATE AND IS THUS VERY DIFFERENT EXCEPT FOR MOTION RULES.

;  FIRST OFF, DON'T LET THE DWARVES FOLLOW HIM INTO A PIT OR A WALL.  ACTIVATE
;  THE WHOLE MESS THE FIRST TIME HE GETS AS FAR AS THE HALL OF MISTS (LOC 15).
;  IF NEWLOC IS FORBIDDEN TO PIRATE (IN PARTICULAR, IF IT'S BEYOND THE TROLL
;  BRIDGE), BYPASS DWARF STUFF.  THAT WAY PIRATE CAN'T STEAL RETURN TOLL, AND
;  DWARVES CAN'T MEET THE BEAR.  ALSO MEANS DWARVES WON'T FOLLOW HIM INTO DEAD
;  END IN MAZE, BUT C'EST LA VIE.  THEY'LL WAIT FOR HIM OUTSIDE THE DEAD END.

  if (LOC eq 0 || _FORCED_(LOC) || _BITSET_(NEWLOC,3)) then goto, L2000
  if (DFLAG ne 0) then goto, L6000
  if (LOC ge 15) then DFLAG = 1
  goto, L2000

;  WHEN WE ENCOUNTER THE FIRST DWARF, WE KILL 0, 1, OR 2 OF THE 5 DWARVES.  IF
;  ANY OF THE SURVIVORS IS AT LOC, REPLACE HIM WITH THE ALTERNATE.

L6000:
  if (DFLAG ne 1) then goto, L6010
  if (LOC lt 15 || _Pct_(95)) then goto, L2000
  DFLAG = 2
  for I = 1,2 do begin
    J = 1 + _RND_(5)
    if (_Pct_(50)) then DLOC[J] = 0
  endfor
  for I = 1,5 do begin
    if (DLOC[I] eq LOC) then DLOC[I] = DALTLC
    ODLOC[I] = DLOC[I]
  endfor
  _RSPEAK_, 3
  _Drop_, AXE, LOC
  goto, L2000

;  THINGS ARE IN FULL SWING.  MOVE EACH DWARF AT RANDOM, EXCEPT IF HE'S SEEN US
;  HE STICKS WITH US.  DWARVES NEVER GO TO LOCS <15.  IF WANDERING AT RANDOM,
;  THEY DON'T BACK UP UNLESS THERE'S NO ALTERNATIVE.  IF THEY DON'T HAVE TO
;  MOVE, THEY ATTACK.  AND, OF COURSE, DEAD DWARVES DON'T DO MUCH OF ANYTHING.

L6010:
  DTOTAL = 0
  ATTACK = 0
  STICK = 0
  for I = 1,6 do begin
    if (DLOC[I] eq 0) then continue
    J = 1
    KK = DLOC[I]
    KK = KEY[KK]
    if (KK eq 0) then goto, L6016
L6012:
    NEWLOC = TRVLOC[KK]
    if (NEWLOC gt 300 || NEWLOC lt 15 || NEWLOC eq ODLOC[I] $
      || (J gt 1 && NEWLOC eq TK[J-1]) || J ge 20 $
      || NEWLOC eq DLOC[I] || _FORCED_(NEWLOC) $
      || (I eq 6 && _BITSET_(NEWLOC,3)) $
      || TRVCON[KK] eq 100) then goto, L6014
    TK[J] = NEWLOC
    J++
L6014:
    KK++
    if (TRAVEL[KK-1] ge 0) then goto, L6012
L6016:
    TK[J] = ODLOC[I]
    if (J ge 2) then J--
    J = 1 + _Rnd_(J)
    ODLOC[I] = DLOC[I]
    DLOC[I] = TK[J]
    DSEEN[I] = (DSEEN[I] && LOC ge 15) $
      || (DLOC[I] eq LOC || ODLOC[I] eq LOC)
    if (~DSEEN[I]) then continue
    DLOC[I] = LOC
    if (I ne 6) then goto, L6027

;  THE PIRATE'S SPOTTED HIM.  HE LEAVES HIM ALONE ONCE WE'VE FOUND CHEST.
;  K COUNTS IF A TREASURE IS HERE.  IF NOT, AND TALLY = TALLY2 PLUS ONE FOR
;  AN UNSEEN CHEST, LET THE PIRATE BE SPOTTED.

    if (LOC eq CHLOC || PROP[CHEST] ge 0) then continue

    K = 0
    for J = 50,MAXTRS do begin

;  PIRATE WON'T TAKE PYRAMID FROM PLOVER ROOM OR DARK ROOM (TOO EASY!).

      if (J eq PYRAM && (LOC eq PLAC[PYRAM] $
                || LOC eq PLAC[EMRALD])) then goto, L6020
      if (_Toting_(J)) then goto, L6022
L6020:
      if (_Here_(J)) then K = 1
    endfor

    if (TALLY eq TALLY2+1 && K eq 0 && PLACE[CHEST] eq 0 $
      && _Here_(LAMP) && PROP[LAMP] eq 1) then goto, L6025
    if (ODLOC[6] ne DLOC[6] && _Pct_(20)) then _RSPEAK_, 127
    continue

L6022:
    _RSPEAK_, 128

;  DON'T STEAL CHEST BACK FROM TROLL!

    if (PLACE[MESSAG] eq 0) then _Move_, CHEST,CHLOC
    _Move_, MESSAG,CHLOC2

    for J = 50,MAXTRS do begin
      if (J eq PYRAM && (LOC eq PLAC[PYRAM] $
                || LOC eq PLAC[EMRALD])) then continue
      if (_At_(J) && FIXED[J] eq 0) then _Carry_, J, LOC
      if (_Toting_(J)) then _Drop_, J,CHLOC
    endfor

L6024:
    DLOC[6] = CHLOC
    ODLOC[6] = CHLOC
    DSEEN[6] = FALSE
    continue

L6025:
    _RSPEAK_, 186
    _Move_, CHEST,CHLOC
    _Move_, MESSAG,CHLOC2
    goto, L6024

;  THIS THREATENING LITTLE DWARF IS IN THE ROOM WITH HIM!

L6027:
    DTOTAL++
    if (ODLOC[I] ne DLOC[I]) then continue
    ATTACK++
    if (KNFLOC ge 0) then KNFLOC = LOC
    if (_Rnd_(1000) lt 95*(DFLAG-2)) then STICK++

  endfor

;  NOW WE KNOW WHAT'S HAPPENING.  LET'S TELL THE POOR SUCKER ABOUT IT.

  if (DTOTAL eq 0) then goto, L2000

  if (DTOTAL eq 1) then begin
    _RSPEAK_, 4
  endif else begin
    _Speak_, 'There are ' + Strtrim(DTOTAL,2) + $
      ' threatening little dwarves in the'
    _Speak_, 'room with you.'
  endelse

  if (ATTACK eq 0) then goto, L2000
  if (DFLAG eq 2) then DFLAG = 3
  if (ATTACK eq 1) then goto, L79
  _Speak_, '' + Strtrim(ATTACK,2) + ' of them throw knives at you!'
  K = 6
L82:
  if (STICK gt 1) then begin
    _Speak_, '' + Strtrim(STICK,2) + ' of them get you!'
  endif else begin
    _RSPEAK_, K + STICK
    if (STICK eq 0) then goto, L2000
  endelse
  OLDLC2 = LOC
  goto, L99

L79:
  _RSPEAK_, 5
  K = 52
  goto, L82

;  DESCRIBE THE CURRENT LOCATION AND [MAYBE] GET NEXT COMMAND.

;  PRINT TEXT FOR CURRENT LOC.

L2000:
  if (LOC eq 0) then goto, L99
  KK = STEXT[LOC]
  KENT = 0
  if (ABBNUM ne 0) then KENT = ABB[LOC] mod ABBNUM
  if (KENT eq 0 || KK eq '') then KK = LTEXT[LOC]
  if (_FORCED_(LOC) || ~_DARK_()) then goto, L2001
  if (WZDARK && _PCT_(35)) then goto, L90
  KK = RTEXT[16]
L2001:
  if (_Toting_(BEAR)) then _RSPEAK_, 141
  _Speak_, KK
  K = 1
  if (_FORCED_(LOC)) then goto, L8
  if (LOC eq 33 && _Pct_(25) && ~CLOSNG) then _RSPEAK_, 8

;  PRINT OUT DESCRIPTIONS OF OBJECTS AT THIS LOCATION.  IF NOT CLOSING AND
;  PROPERTY VALUE IS NEGATIVE, TALLY OFF ANOTHER TREASURE.  RUG IS SPECIAL
;  CASE; ONCE SEEN, ITS PROP IS 1 (DRAGON ON IT) TILL DRAGON IS KILLED.
;  SIMILARLY FOR CHAIN; PROP IS INITIALLY 1 (LOCKED TO BEAR).  THESE HACKS
;  ARE BECAUSE PROP = 0 IS NEEDED TO GET FULL SCORE.

  if (_Dark_()) then goto, L2012
  ABB[LOC]++
  I = ATLOC[LOC]
  debugCounter = 0
L2004:
  debugCounter++
  if (debugCounter gt 101) then Message, 'Infinite loop detected!'
  if (I eq 0) then goto, L2012
  OBJ = I
  if (OBJ gt 100) then OBJ -= 100
  if (OBJ eq STEPS && _TOTING_(NUGGET)) then goto, L2008
  if (PROP[OBJ] ge 0) then goto, L2006
  if (CLOSED) then goto, L2008
  PROP[OBJ] = 0
  if (OBJ eq RUG || OBJ eq CHAIN) then PROP[OBJ] = 1
  TALLY--
  ;  IF REMAINING TREASURES TOO ELUSIVE, ZAP HIS LAMP.
  if (TALLY eq TALLY2 && TALLY ne 0) then LIMIT = Min([35,LIMIT])
L2006:
  KK = PROP[OBJ]
  if (OBJ eq STEPS && LOC eq FIXED[STEPS]) then KK = 1
  _Pspeak_, OBJ, KK
L2008:
  I = LINK[I]
  goto, L2004

L2009:
  K = 54
L2010:
  SPK = K
L2011:
  _RSPEAK_, SPK

L2012:
  VERB = 0
  OBJ = 0

;  CHECK IF THIS LOC IS ELIGIBLE FOR ANY HINTS.  IF BEEN HERE LONG ENOUGH,
;  BRANCH TO HELP SECTION (ON LATER PAGE).  HINTS ALL COME BACK HERE EVENTUALLY
;  TO FINISH THE LOOP.  IGNORE "HINTS" < 4 (SPECIAL STUFF, SEE DATABASE NOTES).

L2600:
  for HINT = 4,HNTMAX do begin
      if (HINTED[HINT]) then continue
      if (~_Bitset_(loc,hint)) then HINTLC[HINT] = -1
      HINTLC[HINT]++
      if (HINTLC[HINT] ge HINTS[HINT,1]) then goto, L40000
L2602:
  endfor

;  KICK THE RANDOM NUMBER GENERATOR JUST TO ADD VARIETY TO THE CHASE.  ALSO,
;  IF CLOSING TIME, CHECK FOR ANY OBJECTS BEING TOTED WITH PROP < 0 AND SET
;  THE PROP TO -1-PROP.  THIS WAY OBJECTS WON'T BE DESCRIBED UNTIL THEY'VE
;  BEEN PICKED UP AND PUT DOWN SEPARATE FROM THEIR RESPECTIVE PILES.  DON'T
;  TICK CLOCK1 UNLESS WELL INTO CAVE (AND NOT AT Y2).

  if (~CLOSED) then goto, L2605
  if (PROP[OYSTER] lt 0 && _Toting_(OYSTER)) then _PSpeak_, OYSTER, 1
  for I = 1,100 do begin
    if (_Toting_(I) && PROP[I] lt 0) then PROP[I] = -1-PROP[I]
  endfor
L2605:
  WZDARK = _Dark_()
  if (KNFLOC gt 0 && KNFLOC ne LOC) then KNFLOC = 0

  if (~DEBUG && WIDBASE gt 0) then return, 1
CMD1:
  _GETIN_, WD1, WD1A, WD1X, WD2, WD2A, WD2X

;  EVERY INPUT, CHECK "FOOBAR" FLAG.  IF ZERO, NOTHING'S GOING ON.  IF POS,
;  MAKE NEG.  IF NEG, HE SKIPPED A WORD, SO MAKE IT ZERO.

L2608:
  FOOBAR = MIN([0,-FOOBAR])
  TURNS++
;     if (VERB eq SAY && WD2 ne 0) then VERB = 0
;     if (VERB eq SAY) then goto, L4090
  if (TALLY eq 0 && LOC ge 15 && LOC ne 33) then CLOCK1--
  if (CLOCK1 eq 0) then goto, L10000
  if (CLOCK1 lt 0) then CLOCK2--
  if (CLOCK2 eq 0) then goto, L11000
  if (PROP[LAMP] eq 1) then LIMIT--
  if (LIMIT le 30 && _HERE_(BATTER) && PROP[BATTER] eq 0 $
    && _HERE_(LAMP)) then goto, L12000
  if (LIMIT eq 0) then goto, L12400
  if (LIMIT lt 0 && LOC le 8) then goto, L12600
  if (LIMIT le 30) then goto, L12200
L19999:
  K = 43
  if (_LIQLOC_() eq WATER) then K = 70

;  DO PRELIMINARY ANALYSIS OF SENTENCE TO FIND CERTAIN SPECIAL
;  CASES, VIZ,

;  ENTER <WATER,STREAM>
;  ENTER <LOCATION>
;  <WATER,OIL> <PLANT,DOOR>

  _Vocab_, WD1, WD1A, -1, I
  _Vocab_, WD2, WD2A, -1, J

  if (WD1 ne CEN || WD1A ne CTE) then goto, L2609
  if (J eq (WATER+1000) || J eq STREAM) then goto, L2010
  if (WD2 ne '') then goto, L2800
L2609:
  if ((I ne (WATER+1000) && I ne (OIL+1000)) $
    || (J ne (PLANT+1000) && J ne (DOOR+1000))) then goto, L2610
  WD2 = CPO
  WD2A = CUR
L2610:
  if (WD1 eq CWE && WD1A eq CST && _PCT_(10)) then _RSPEAK_, 17
L2630:
  _Vocab_, WD1, WD1A, -1, I
  if (I eq -1) then goto, L3000
  K = I mod 1000
  KQ = I/1000 + 1
  case KQ of
  1: goto, L8
  2: goto, L5000
  3: goto, L4000
  4: goto, L2010
  else: Message, 'VOCABULARY TYPE (N/1000) NOT BETWEEN 0 AND 3'
  endcase


;  GET SECOND WORD FOR ANALYSIS.

L2800:
  WD1 = WD2
  WD1A = WD2A
  WD1X = WD2X
  WD2 = 0
  goto, L2610

;  GEE, I DON'T UNDERSTAND.

L3000:
  SPK = 60
  if (_PCT_(20)) then SPK = 61
  if (_PCT_(20)) then SPK = 13
  _RSPEAK_, SPK
  goto, L2600

;  ANALYSE A VERB.  REMEMBER WHAT IT WAS, GO BACK FOR OBJECT IF SECOND WORD
;  UNLESS VERB IS "SAY", WHICH SNARFS ARBITRARY SECOND WORD.

L4000:
  VERB = K
  SPK = ACTSPK[VERB]
  if (WD2 ne '' && VERB ne SAY) then goto, L2800
  if (VERB eq SAY) then OBJ = WD2
  if (Size(OBJ,/TYPE) ne 7) then begin
    if (OBJ ne 0) then goto, L4090
  endif else begin
    if (OBJ ne '') then goto, L4090
  endelse

;  ANALYSE AN INTRANSITIVE VERB (IE, NO OBJECT GIVEN YET).

L4080:
  case (verb) of
   1: goto, L8010 ; TAKE
   2: goto, L8000 ; DROP
   3: goto, L8000 ; SAY
   4: goto, L8040 ; OPEN
   5: goto, L2009 ; NOTH
   6: goto, L8040 ; LOCK
   7: goto, L9070 ; ON
   8: goto, L9080 ; OFF
   9: goto, L8000 ; WAVE
  10: goto, L8000 ; CALM
  11: goto, L2011 ; WALK
  12: goto, L9120 ; KILL
  13: goto, L9130 ; POUR
  14: goto, L8140 ; EAT
  15: goto, L9150 ; DRNK
  16: goto, L8000 ; RUB
  17: goto, L8000 ; TOSS
  18: goto, L8180 ; QUIT
  19: goto, L8000 ; FIND
  20: goto, L8200 ; INVN
  21: goto, L8000 ; FEED
  22: goto, L9220 ; FILL
  23: goto, L9230 ; BLST
  24: goto, L8240 ; SCOR
  25: goto, L8250 ; FOO
  26: goto, L8260 ; BRF
  27: goto, L8270 ; READ
  28: goto, L8000 ; BREK
  29: goto, L8000 ; WAKE
  30: goto, L8300 ; SUSP
  31: goto, L8310 ; HOUR
  32: goto, L8320 ; RESU
  else: Message, 'INTRANSITIVE ACTION VERB EXCEEDS LIST'
  endcase


;  ANALYSE A TRANSITIVE VERB.

L4090:
  case (verb) of
   1: goto, L9010 ; TAKE
   2: goto, L9020 ; DROP
   3: goto, L9030 ; SAY
   4: goto, L9040 ; OPEN
   5: goto, L2009 ; NOTH
   6: goto, L9040 ; LOCK
   7: goto, L9070 ; ON
   8: goto, L9080 ; OFF
   9: goto, L9090 ; WAVE
  10: goto, L2011 ; CALM
  11: goto, L2011 ; WALK
  12: goto, L9120 ; KILL
  13: goto, L9130 ; POUR
  14: goto, L9140 ; EAT
  15: goto, L9150 ; DRNK
  16: goto, L9160 ; RUB
  17: goto, L9170 ; TOSS
  18: goto, L2011 ; QUIT
  19: goto, L9190 ; FIND
  20: goto, L9190 ; INVN
  21: goto, L9210 ; FEED
  22: goto, L9220 ; FILL
  23: goto, L9230 ; BLST
  24: goto, L2011 ; SCOR
  25: goto, L2011 ; FOO
  26: goto, L2011 ; BRF
  27: goto, L9270 ; READ
  28: goto, L9280 ; BREK
  29: goto, L9290 ; WAKE
  30: goto, L2011 ; SUSP
  31: goto, L2011 ; HOUR
  32: goto, L2011 ; RESU
  else: Message, 'TRANSITIVE ACTION VERB EXCEEDS LIST'
  endcase

;  ANALYSE AN OBJECT WORD.  SEE IF THE THING IS HERE, WHETHER WE'VE GOT A VERB
;  YET, AND SO ON.  OBJECT MUST BE HERE UNLESS VERB IS "FIND" OR "INVENT[ORY]"
;  (AND NO NEW VERB YET TO BE ANALYSED).  WATER AND OIL ARE ALSO FUNNY, SINCE
;  THEY ARE NEVER ACTUALLY DROPPED AT ANY LOCATION, BUT MIGHT BE HERE INSIDE
;  THE BOTTLE OR AS A FEATURE OF THE LOCATION.

L5000:
  OBJ = K
  if (FIXED[K] ne LOC && ~_Here_(K)) then goto, L5100
L5010:
  if (WD2 ne '') then goto, L2800
  if (VERB ne 0) then goto, L4090
  _Speak_, 'What do you want to do with the '+WD1+WD1A+WD1X+'?'
  goto, L2600

L5100:
  if (K ne GRATE) then goto, L5110
  if (LOC eq 1 || LOC eq 4 || LOC eq 7) then K = DPRSSN
  if (LOC gt 9 && LOC lt 15) then K = ENTRNC
  if (K ne GRATE) then goto, L8
L5110:
  if (K ne DWARF) then goto, L5120
  for I = 1,5 do begin
    if (DLOC[I] eq LOC && DFLAG ge 2) then goto, L5010
  endfor
L5120:
  if ((_Liq_() eq K && _Here_(BOTTLE)) || K eq _Liqloc_()) then goto, L5010
  if (OBJ ne PLANT || ~_At_(PLANT2) || PROP[PLANT2] eq 0) then goto, L5130
  OBJ = PLANT2
  goto, L5010
L5130:
  if (OBJ ne KNIFE || KNFLOC ne LOC) then goto, L5140
  KNFLOC = -1
  SPK = 116
  goto, L2011
L5140:
  if (OBJ ne ROD || ~_HERE_(ROD2)) then goto, L5190
  OBJ = ROD2
  goto, L5010
L5190:
  if ((VERB eq FIND || VERB eq INVENT) && WD2 eq '') then goto, L5010
  _Speak_, 'I don''t see any '+WD1+WD1A+WD1X+'.'
  goto, L2012

;  FIGURE OUT THE NEW LOCATION

;  GIVEN THE CURRENT LOCATION IN "LOC", AND A MOTION VERB NUMBER IN "K", PUT
;  THE NEW LOCATION IN "NEWLOC".  THE CURRENT LOC IS SAVED IN "OLDLOC" IN CASE
;  HE WANTS TO RETREAT.  THE CURRENT OLDLOC IS SAVED IN OLDLC2, IN CASE HE
;  DIES.  (IF HE DOES, NEWLOC WILL BE LIMBO, AND OLDLOC WILL BE WHAT KILLED
;  HIM, SO WE NEED OLDLC2, WHICH IS THE LAST PLACE HE WAS SAFE.)

L8:
  KK = KEY[LOC]
  NEWLOC = LOC
  if (KK eq 0) then Message, 'LOCATION HAS NO TRAVEL ENTRIES'
  if (K eq NULL) then goto, L2
  if (K eq BACK) then goto, L20
  if (K eq LOOK) then goto, L30
  if (K eq CAVE) then goto, L40
  OLDLC2 = OLDLOC
  OLDLOC = LOC

L9:
  LL = Abs(TRAVEL[KK])
  if (LL eq 1 || LL eq K) then goto, L10
  if (TRAVEL[KK] lt 0) then goto, L50
  KK++
  goto, L9

L10:
  NEWLOC= TRVCON[KK]
  K = NEWLOC mod 100
  if (NEWLOC le 300) then goto, L13
  if (PROP[K] ne NEWLOC/100-3) then goto, L16

;  TRY NEXT ENTRY IN TRAVEL TABLE

L12:
  if (TRAVEL[KK] lt 0) then Message, 'CONDITIONAL TRAVEL ENTRY WITH NO ALTERNATIVE'
  KK++

;  MAKE SURE HE DOESN'T GO THROUGH SAME TEST AGAIN

  if (TRVCON[KK-1] eq TRVCON[KK] && TRVLOC[KK-1] eq TRVLOC[KK]) then goto, L12
  goto, L10

L13:
  if (NEWLOC le 100) then goto, L14
  if (_TOTING_(K) || (NEWLOC gt 200 && _AT_(K))) then goto, L16
  goto, L12

L14:
  if (NEWLOC ne 0 && ~_Pct_(NEWLOC)) then goto, L12
L16:
  NEWLOC = TRVLOC[KK]
  if (NEWLOC le 300) then goto, L2
  if (NEWLOC le 500) then goto, L30000
  _RSPEAK_, NEWLOC - 500
  NEWLOC = LOC
  goto, L2


;  SPECIAL MOTIONS COME HERE.  LABELLING CONVENTION: STATEMENT NUMBERS NNNXX
;  (XX = 00-99) ARE USED FOR SPECIAL CASE NUMBER NNN (NNN = 301-500).

L30000:
  NEWLOC = NEWLOC-300
  case (NEWLOC) of
  1: goto, L30100
  2: goto, L30200
  3: goto, L30300
  else: Message, 'SPECIAL TRAVEL (500>L>300) EXCEEDS LIST'
  endcase

;  TRAVEL 301.  PLOVER-ALCOVE PASSAGE.  CAN CARRY ONLY EMERALD.  NOTE: TRAVEL
;  TABLE MUST INCLUDE "USELESS" ENTRIES GOING THROUGH PASSAGE, WHICH CAN NEVER
;  BE USED FOR ACTUAL MOTION, BUT CAN BE SPOTTED BY "GO BACK".

L30100:
  NEWLOC = 99+100-LOC
  if (HOLDNG eq 0 || (HOLDNG eq 1 && _TOTING_(EMRALD))) then goto, L2
  NEWLOC = LOC
  _RSPEAK_, 117
  goto, L2

;  TRAVEL 302.  PLOVER TRANSPORT.  DROP THE EMERALD (ONLY USE SPECIAL TRAVEL IF
;  TOTING IT), SO HE'S FORCED TO USE THE PLOVER-PASSAGE TO GET IT OUT.  HAVING
;  DROPPED IT, GO BACK AND PRETEND HE WASN'T CARRYING IT AFTER ALL.

L30200:
  _Drop_, EMRALD, LOC
  goto, L12

;  TRAVEL 303.  TROLL BRIDGE.  MUST BE DONE ONLY AS SPECIAL MOTION SO THAT
;  DWARVES WON'T WANDER ACROSS AND ENCOUNTER THE BEAR.  (THEY WON'T FOLLOW THE
;  PLAYER THERE BECAUSE THAT REGION IS FORBIDDEN TO THE PIRATE.)  IF
;  PROP[TROLL] = 1, HE'S CROSSED SINCE PAYING, SO STEP OUT AND BLOCK HIM.
;  (STANDARD TRAVEL ENTRIES CHECK FOR PROP[TROLL] = 0.)  SPECIAL STUFF FOR BEAR.

L30300:
  if (PROP[TROLL] ne 1) then goto, L30310
  _PSPEAK_, TROLL, 1
  PROP[TROLL] = 0
  _MOVE_, TROLL2, 0
  _MOVE_, TROLL2+100,0
  _MOVE_, TROLL,PLAC[TROLL]
  _MOVE_, TROLL+100,FIXD[TROLL]
  _JUGGLE_, CHASM
  NEWLOC = LOC
  goto,L2

L30310:
  NEWLOC= PLAC[TROLL]+FIXD[TROLL]-LOC
  if (PROP[TROLL] eq 0) then PROP[TROLL] = 1
  if (~_TOTING_(BEAR)) then goto, L2
  _RSPEAK_, 162
  PROP[CHASM] = 1
  PROP[TROLL] = 2
  _DROP_, BEAR, NEWLOC
  FIXED[BEAR] = -1
  PROP[BEAR] = 3
  if (PROP[SPICES] lt 0) then TALLY2++
  OLDLC2 = NEWLOC
  goto, L99

;  END OF SPECIALS.

;  HANDLE "GO BACK".  LOOK FOR VERB WHICH GOES FROM LOC TO OLDLOC, OR TO OLDLC2
;  IF OLDLOC HAS FORCED-MOTION.  K2 SAVES ENTRY -> FORCED LOC -> PREVIOUS LOC.

L20:
  K = OLDLOC
  if (_FORCED_(k)) then K = OLDLC2
  OLDLC2 = OLDLOC
  OLDLOC = LOC
  K2 = 0
  if (K ne LOC) then goto, L21
  _RSPEAK_, 91
  goto, L2

L21:
  LL = TRVLOC[KK]
  if (LL eq K) then goto, L25
  if (LL gt 300) then goto, L22
  J = KEY[LL]
  if (_FORCED_(ll) && TRVLOC[KK] eq K) then K2 = KK
L22:
  if (TRAVEL[KK] lt 0) then goto, L23
  KK = KK+1
  goto, L21

L23:
  KK = K2
  if (KK ne 0) then goto, L25
  _RSPEAK_, 140
  goto, L2

L25:
  K = Abs(TRAVEL[KK])
  KK = KEY[LOC]
  goto, L9

;  LOOK.  CAN'T GIVE MORE DETAIL.  PRETEND IT WASN'T DARK (THOUGH IT MAY "NOW"
;  BE DARK) SO HE WON'T FALL INTO A PIT WHILE STARING INTO THE GLOOM.

L30:
  if (DETAIL lt 3) then _RSPEAK_, 15
  DETAIL++
  WZDARK = FALSE
  ABB[LOC] = 0
  goto, L2

;  CAVE.  DIFFERENT MESSAGES DEPENDING ON WHETHER ABOVE GROUND.

L40:
  if (LOC lt 8) then _RSPEAK_, 57
  if (LOC ge 8) then _RSPEAK_, 58
  goto, L2

;  NON-APPLICABLE MOTION.  VARIOUS MESSAGES DEPENDING ON WORD GIVEN.

L50:
  SPK = 12
  if (K ge 43 && K le 50) then SPK = 9
  if (K eq 29 || K eq 30) then SPK = 9
  if (K eq 7 || K eq 36 || K eq 37) then SPK = 10
  if (K eq 11 || K eq 19) then SPK = 11
  if (VERB eq FIND || VERB eq INVENT) then SPK = 59
  if (K eq 62 || K eq 65) then SPK = 42
  if (K eq 17) then SPK = 80
  _RSPEAK_, SPK
  goto, L2

;  "YOU'RE DEAD, JIM."

;  IF THE CURRENT LOC IS ZERO, IT MEANS THE CLOWN GOT HIMSELF KILLED.  WE'LL
;  ALLOW THIS MAXDIE TIMES.  MAXDIE IS AUTOMATICALLY SET BASED ON THE NUMBER OF
;  SNIDE MESSAGES AVAILABLE.  EACH DEATH RESULTS IN A MESSAGE (81, 83, ETC.)
;  WHICH OFFERS REINCARNATION; IF ACCEPTED, THIS RESULTS IN MESSAGE 82, 84,
;  ETC.  THE LAST TIME, IF HE WANTS ANOTHER CHANCE, HE GETS A SNIDE REMARK AS
;  WE EXIT.  WHEN REINCARNATED, ALL OBJECTS BEING CARRIED GET DROPPED AT OLDLC2
;  (PRESUMABLY THE LAST PLACE PRIOR TO BEING KILLED) WITHOUT CHANGE OF PROPS.
;  THE LOOP RUNS BACKWARDS TO ASSURE THAT THE BIRD IS DROPPED BEFORE THE CAGE.
;  (THIS KLUGE COULD BE CHANGED ONCE WE'RE SURE ALL REFERENCES TO BIRD AND CAGE
;  ARE DONE BY KEYWORDS.)  THE LAMP IS A SPECIAL CASE (IT WOULDN'T DO TO LEAVE
;  IT IN THE CAVE).  IT IS TURNED OFF AND LEFT OUTSIDE THE BUILDING (ONLY IF HE
;  WAS CARRYING IT, OF COURSE).  HE HIMSELF IS LEFT INSIDE THE BUILDING (AND
;  HEAVEN HELP HIM IF HE TRIES TO XYZZY BACK INTO THE CAVE WITHOUT THE LAMP!).
;  OLDLOC IS ZAPPED SO HE CAN'T JUST "RETREAT".

;  THE EASIEST WAY TO GET KILLED IS TO FALL INTO A PIT IN PITCH DARKNESS.

L90:
  _RSPEAK_, 23
  OLDLC2 = LOC

;  OKAY, HE'S DEAD.  LET'S GET ON WITH IT.

L99:
  if (CLOSNG) then goto, L95
  _RSPEAK_, 81+NUMDIE*2
   if (~DEBUG && WIDBASE gt 0) then return, 4
CMD4:
  YEA = _YES_(81+NUMDIE*2,82+NUMDIE*2,54)
  NUMDIE++
  if (NUMDIE eq MAXDIE || ~YEA) then goto, L20000
  PLACE[WATER] = 0
  PLACE[OIL] = 0
  if (_Toting_(LAMP)) then PROP[LAMP] = 0
  for J = 1,100 do begin
    I = 101 - J
    if (~_Toting_(I)) then continue
    K = OLDLC2
    if (I eq LAMP) then K = 1
    _Drop_, I, K
  endfor
  LOC = 3
  OLDLOC = LOC
  goto, L2000

;  HE DIED DURING CLOSING TIME.  NO RESURRECTION.  TALLY UP A DEATH AND EXIT.

L95:
  _RSPEAK_, 131
  NUMDIE++
  goto, L20000

;  ROUTINES FOR PERFORMING THE VARIOUS ACTION VERBS

;  STATEMENT NUMBERS IN THIS SECTION ARE 8000 FOR INTRANSITIVE VERBS, 9000 FOR
;  TRANSITIVE, PLUS TEN TIMES THE VERB NUMBER.  MANY INTRANSITIVE VERBS USE THE
;  TRANSITIVE CODE, AND SOME VERBS USE CODE FOR OTHER VERBS, AS NOTED BELOW.

;  RANDOM INTRANSITIVE VERBS COME HERE.  CLEAR OBJ JUST IN CASE (SEE "ATTACK").

L8000:
  _Speak_, 'I don''t understand "'+WD1+WD1A+WD1X+'".'
  OBJ = 0
  goto, L2600

;  CARRY, NO OBJECT GIVEN YET.  OK IF ONLY ONE OBJECT PRESENT.

L8010:
  if (ATLOC[LOC] eq 0 || LINK[ATLOC[LOC]] ne 0) then goto, L8000
  for I = 1,5 do begin
    if (DLOC[I] eq LOC && DFLAG ge 2) then goto, L8000
  endfor
  OBJ = ATLOC[LOC]
  if (OBJ gt 100) then OBJ -= 100  ; CT, added

;  CARRY AN OBJECT.  SPECIAL CASES FOR BIRD AND CAGE (IF BIRD IN CAGE, CAN'T
;  TAKE ONE WITHOUT THE OTHER.  LIQUIDS ALSO SPECIAL, SINCE THEY DEPEND ON
;  STATUS OF BOTTLE.  ALSO VARIOUS SIDE EFFECTS, ETC.

L9010:
  if (_Toting_(OBJ)) then goto, L2011
  SPK = 25
  if (OBJ eq PLANT && PROP[PLANT] le 0) then SPK = 115
  if (OBJ eq BEAR && PROP[BEAR] eq 1) then SPK = 169
  if (OBJ eq CHAIN && PROP[BEAR] ne 0) then SPK = 170
  if (FIXED[OBJ] ne 0) then goto, L2011
  if (OBJ ne WATER && OBJ ne OIL) then goto, L9017
  if (_Here_(BOTTLE) && _Liq_() eq OBJ) then goto, L9018
  OBJ = BOTTLE
  if (_Toting_(BOTTLE) && PROP[BOTTLE] eq 1) then goto, L9220
  if (PROP[BOTTLE] ne 1) then SPK = 105
  if (~_Toting_(BOTTLE)) then SPK = 104
  goto, L2011
L9018:
  OBJ = BOTTLE
L9017:
  if (HOLDNG lt 7) then goto, L9016
  _RSPEAK_, 92
  goto, L2012
L9016:
  if (OBJ ne BIRD) then goto, L9014
  if (PROP[BIRD] ne 0) then goto, L9014
  if (~_Toting_(ROD)) then goto, L9013
  _RSPEAK_, 26
  goto, L2012
L9013:
  if (_Toting_(CAGE)) then goto, L9015
  _RSPEAK_, 27
  goto, L2012
L9015:
  PROP[BIRD] = 1
L9014:
  if ((OBJ eq BIRD || OBJ eq CAGE) && PROP[BIRD] ne 0) then begin
    _Carry_, BIRD + CAGE - OBJ, LOC
  endif
  _Carry_, OBJ, LOC
  K = _Liq_()
  if (OBJ eq BOTTLE && K ne 0) then PLACE[K] = -1
  goto, L2009

;  DISCARD OBJECT.  "THROW" ALSO COMES HERE FOR MOST OBJECTS.  SPECIAL CASES FOR
;  BIRD (MIGHT ATTACK SNAKE OR DRAGON) AND CAGE (MIGHT CONTAIN BIRD) AND VASE.
;  DROP COINS AT VENDING MACHINE FOR EXTRA BATTERIES.

L9020:
  if (_Toting_(ROD2) && OBJ eq ROD && ~_Toting_(ROD)) then OBJ = ROD2
  if (~_Toting_(OBJ)) then goto, L2011
  if (OBJ ne BIRD || ~_Here_(SNAKE)) then goto, L9024
  _RSPEAK_, 30
  if (CLOSED) then goto, L19000
  _DSTROY_, SNAKE

;  SET PROP FOR USE BY TRAVEL OPTIONS

  PROP[SNAKE] = 1
L9021:
  K = _Liq_()
  if (K eq OBJ) then OBJ = BOTTLE
  if (OBJ eq BOTTLE && K ne 0) then PLACE[K] = 0
  if (OBJ eq CAGE && PROP[BIRD] ne 0) then _Drop_, BIRD, LOC
  if (OBJ eq BIRD) then PROP[BIRD] = 0
  _Drop_, OBJ, LOC
  goto, L2012

L9024:
  if (OBJ ne COINS || ~_Here_(VEND)) then goto, L9025
  _DSTROY_, COINS
  _Drop_, BATTER, LOC
  _PSpeak_, BATTER, 0
  goto, L2012

L9025:
  if (OBJ ne BIRD || ~_At_(DRAGON) || PROP[DRAGON] ne 0) then goto, L9026
  _RSPEAK_, 154
  _DSTROY_, BIRD
  PROP[BIRD] = 0
  if (PLACE[SNAKE] eq PLAC[SNAKE]) then TALLY2++
  goto, L2012

L9026:
  if (OBJ ne BEAR || ~_At_(TROLL)) then goto, L9027
  _RSPEAK_, 163
  _Move_, TROLL, 0
  _Move_, TROLL+100, 0
  _Move_, TROLL2, PLAC[TROLL]
  _Move_, TROLL2+100, FIXD[TROLL]
  _JUGGLE_, CHASM
  PROP[TROLL] = 2
  goto, L9021

L9027:
  if (OBJ eq VASE && LOC ne PLAC[PILLOW]) then goto, L9028
  _RSPEAK_, 54
  goto, L9021

L9028:
  PROP[VASE] = 2
  if (_At_(PILLOW)) then PROP[VASE] = 0
  _PSpeak_, VASE, PROP[VASE]+1
  if (PROP[VASE] ne 0) then FIXED[VASE] = -1
  goto, L9021

;  SAY.  ECHO WD2 (OR WD1 IF NO WD2 (SAY WHAT?, ETC.).)  MAGIC WORDS OVERRIDE.

L9030:
  if (WD2 eq '') then goto, L9031
  WD1 = WD2
  WD1A = WD2A
  WD1X = WD2X
L9031:
  _Vocab_, WD1,WD1A,-1,I
  if (I eq 62 || I eq 65 || I eq 71 || I eq 2025) then goto, L9035
  _Speak_, 'Okay, "'+WD1+WD1A+WD1X+'".'
  goto, L2012

L9035:
  WD2 = 0
  OBJ = 0
  goto, L2630

;  LOCK, UNLOCK, NO OBJECT GIVEN.  ASSUME VARIOUS THINGS IF PRESENT.

L8040:
  SPK = 28
  if (_Here_(CLAM)) then OBJ = CLAM
  if (_Here_(OYSTER)) then OBJ = OYSTER
  if (_At_(DOOR)) then OBJ = DOOR
  if (_At_(GRATE)) then OBJ = GRATE
  if (OBJ ne 0 && _Here_(CHAIN)) then goto, L8000
  if (_Here_(CHAIN)) then OBJ = CHAIN
  if (OBJ eq 0) then goto, L2011

;  LOCK, UNLOCK OBJECT.  SPECIAL STUFF FOR OPENING CLAM/OYSTER AND FOR CHAIN.

L9040:
  if (OBJ eq CLAM || OBJ eq OYSTER) then goto, L9046
  if (OBJ eq DOOR) then SPK = 111
  if (OBJ eq DOOR && PROP[DOOR] eq 1) then SPK = 54
  if (OBJ eq CAGE) then SPK = 32
  if (OBJ eq KEYS) then SPK = 55
  if (OBJ eq GRATE || OBJ eq CHAIN) then SPK = 31
  if (SPK ne 31 || ~_Here_(KEYS)) then goto, L2011
  if (OBJ eq CHAIN) then goto, L9048
  if (~CLOSNG) then goto, L9043
  K = 130
  if (~PANIC) then CLOCK2 = 15
  PANIC = TRUE
  goto, L2010

L9043:
  K = 34 + PROP[GRATE]
  PROP[GRATE] = 1
  if (VERB eq LOCK) then PROP[GRATE] = 0
  K = K + 2*PROP[GRATE]
  goto, L2010

;  CLAM/OYSTER.

L9046:
  K = 0
  if (OBJ eq OYSTER) then K = 1
  SPK = 124+K
  if (_Toting_(OBJ)) then SPK = 120+K
  if (~_Toting_(TRIDNT)) then SPK = 122+K
  if (VERB eq LOCK) then SPK = 61
  if (SPK ne 124) then goto, L2011
  _DSTROY_, CLAM
  _Drop_, OYSTER, LOC
  _Drop_, PEARL, 105
  goto, L2011

;  CHAIN.

L9048:
  if (VERB eq LOCK) then goto, L9049
  SPK = 171
  if (PROP[BEAR] eq 0) then SPK = 41
  if (PROP[CHAIN] eq 0) then SPK = 37
  if (SPK ne 171) then goto, L2011
  PROP[CHAIN] = 0
  FIXED[CHAIN] = 0
  if (PROP[BEAR] ne 3) then PROP[BEAR] = 2
  FIXED[BEAR] = 2-PROP[BEAR]
  goto, L2011

L9049:
  SPK = 172
  if (PROP[CHAIN] ne 0) then SPK = 34
  if (LOC ne PLAC[CHAIN]) then SPK = 173
  if (SPK ne 172) then goto, L2011
  PROP[CHAIN] = 2
  if (_Toting_(CHAIN)) then _Drop_, CHAIN, LOC
  FIXED[CHAIN] = -1
  goto, L2011

;  LIGHT LAMP

L9070:
  if (~_Here_(LAMP)) then goto, L2011
  SPK = 184
  if (LIMIT lt 0) then goto, L2011
  PROP[LAMP] = 1
  _RSPEAK_, 39
  if (WZDARK) then goto, L2000
  goto, L2012

;  LAMP OFF

L9080:
  if (~_Here_(LAMP)) then goto, L2011
  PROP[LAMP] = 0
  _RSPEAK_, 40
  if (_Dark_()) then _RSPEAK_, 16
  goto, L2012

;  WAVE.  NO EFFECT UNLESS WAVING ROD AT FISSURE.

L9090:
  if ((~_Toting_(OBJ)) && (OBJ ne ROD || ~_Toting_(ROD2))) then SPK = 29
  if (OBJ ne ROD || ~_At_(FISSUR) || ~_Toting_(OBJ) $
        || CLOSNG) then goto, L2011
  PROP[FISSUR] = 1 - PROP[FISSUR]
  _PSPEAK_, FISSUR, 2 - PROP[FISSUR]
  goto, L2012

;  ATTACK.  ASSUME TARGET IF UNAMBIGUOUS.  "THROW" ALSO LINKS HERE.  ATTACKABLE
;  OBJECTS FALL INTO TWO CATEGORIES: ENEMIES (SNAKE, DWARF, ETC.)  AND OTHERS
;  (BIRD, CLAM).  AMBIGUOUS IF TWO ENEMIES, OR IF NO ENEMIES BUT TWO OTHERS.

L9120:
  for I = 1,5 do begin
    if (DLOC[I] eq LOC && DFLAG ge 2) then goto, L9122
  endfor
  I = 0
L9122:
  if (OBJ ne 0) then goto, L9124
  if (I ne 0) then OBJ = DWARF
  if (_HERE_(SNAKE)) then OBJ = OBJ*100+SNAKE
  if (_AT_(DRAGON) && PROP[DRAGON] eq 0) then OBJ = OBJ*100+DRAGON
  if (_AT_(TROLL)) then OBJ = OBJ*100+TROLL
  if (_HERE_(BEAR) && PROP[BEAR] eq 0) then OBJ = OBJ*100+BEAR
  if (OBJ gt 100) then goto, L8000
  if (OBJ ne 0) then goto, L9124

;  CAN'T ATTACK BIRD BY THROWING AXE.

  if (_Here_(BIRD) && VERB ne THROW) then OBJ = BIRD

;  CLAM AND OYSTER BOTH TREATED AS CLAM FOR INTRANSITIVE CASE; NO HARM DONE.

  if (_Here_(CLAM) || _Here_(OYSTER)) then OBJ = 100*OBJ+CLAM
  if (OBJ gt 100) then goto, L8000
L9124:
  if (OBJ ne BIRD) then goto, L9125
  SPK = 137
  if (CLOSED) then goto, L2011
  _DSTROY_, BIRD
  PROP[BIRD] = 0
  if (PLACE[SNAKE] eq PLAC[SNAKE]) then TALLY2++
  SPK = 45
L9125:
  if (OBJ eq 0) then SPK = 44
  if (OBJ eq CLAM || OBJ eq OYSTER) then SPK = 150
  if (OBJ eq SNAKE) then SPK = 46
  if (OBJ eq DWARF) then SPK = 49
  if (OBJ eq DWARF && CLOSED) then goto, L19000
  if (OBJ eq DRAGON) then SPK = 167
  if (OBJ eq TROLL) then SPK = 157
  if (OBJ eq BEAR) then SPK = 165+(PROP[BEAR]+1)/2
  if (OBJ ne DRAGON || PROP[DRAGON] ne 0) then goto, L2011

;  FUN STUFF FOR DRAGON.  IF HE INSISTS ON ATTACKING IT, WIN!  SET PROP TO DEAD,
;  MOVE DRAGON TO CENTRAL LOC (STILL FIXED), MOVE RUG THERE (NOT FIXED), AND
;  MOVE HIM THERE, TOO.  THEN DO A NULL MOTION TO GET NEW DESCRIPTION.

  _RSPEAK_, 49
  VERB = 0
  OBJ = 0

  if (~DEBUG && WIDBASE gt 0) then return, 2
CMD2:
  _GETIN_, WD1, WD1A, WD1X, WD2, WD2A, WD2X
  if (WD1 ne CYE && WD1 ne CY) then goto, L2608
  _PSpeak_, DRAGON, 1
  PROP[DRAGON] = 2
  PROP[RUG] = 0
  K = (PLAC[DRAGON]+FIXD[DRAGON])/2
  _Move_, DRAGON+100, -1
  _Move_, RUG+100, 0
  _Move_, DRAGON, K
  _Move_, RUG, K
  for OBJ = 1,100 do begin
    if (PLACE[OBJ] eq PLAC[DRAGON] || PLACE[OBJ] eq FIXD[DRAGON]) then begin
       _Move_, OBJ, K
    endif
  endfor
  LOC = K
  K = NULL
  goto, L8

;  POUR.  IF NO OBJECT, OR OBJECT IS BOTTLE, ASSUME CONTENTS OF BOTTLE.
;  SPECIAL TESTS FOR POURING WATER OR OIL ON PLANT OR RUSTY DOOR.

L9130:
  if (OBJ eq BOTTLE || OBJ eq 0) then OBJ = _Liq_()
  if (OBJ eq 0) then goto, L8000
  if (~_Toting_(OBJ)) then goto, L2011
  SPK = 78
  if (OBJ ne OIL && OBJ ne WATER) then goto, L2011
  PROP[BOTTLE] = 1
  PLACE[OBJ] = 0
  SPK = 77
  if (~(_At_(PLANT) || _At_(DOOR))) then goto, L2011

  if (_At_(DOOR)) then goto, L9132
  SPK = 112
  if (OBJ ne WATER) then goto, L2011
  _PSpeak_, PLANT, PROP[PLANT]+1
  PROP[PLANT] = (PROP[PLANT]+2) mod 6
  PROP[PLANT2] = PROP[PLANT]/2
  K = NULL
  goto, L8

L9132:
  PROP[DOOR] = 0
  if (OBJ eq OIL) then PROP[DOOR] = 1
  SPK = 113 + PROP[DOOR]
  goto, L2011

;  EAT.  INTRANSITIVE: ASSUME FOOD IF PRESENT, ELSE ASK WHAT.  TRANSITIVE: FOOD
;  OK, SOME THINGS LOSE APPETITE, REST ARE RIDICULOUS.

L8140:
  if (~_Here_(FOOD)) then goto, L8000
L8142:
  _DSTROY_, FOOD
  SPK = 72
  goto, L2011

L9140:
  if (OBJ eq FOOD) then goto, L8142
  if (OBJ eq BIRD || OBJ eq SNAKE || OBJ eq CLAM || OBJ eq OYSTER $
        || OBJ eq DWARF || OBJ eq DRAGON || OBJ eq TROLL $
        || OBJ eq BEAR) then SPK = 71
  goto, L2011

;  DRINK.  IF NO OBJECT, ASSUME WATER AND LOOK FOR IT HERE.  IF WATER IS IN
;  THE BOTTLE, DRINK THAT, ELSE MUST BE AT A WATER LOC, SO DRINK STREAM.

L9150:  if (OBJ eq 0 && _Liqloc_() ne WATER && (_Liq_() ne WATER $
        || ~_Here_(BOTTLE))) then goto, L8000
  if (OBJ ne 0 && OBJ ne WATER) then SPK = 110
  if (SPK eq 110 || _Liq_() ne WATER || ~_Here_(BOTTLE)) then goto, L2011
  PROP[BOTTLE] = 1
  PLACE[WATER] = 0
  SPK = 74
  goto, L2011

;  RUB.  YIELDS VARIOUS SNIDE REMARKS.

L9160:
  if (OBJ ne LAMP) then SPK = 76
  goto, L2011

;  THROW.  SAME AS DISCARD UNLESS AXE.  THEN SAME AS ATTACK EXCEPT IGNORE BIRD,
;  AND IF DWARF IS PRESENT THEN ONE MIGHT BE KILLED.  (ONLY WAY TO DO SO!)
;  AXE ALSO SPECIAL FOR DRAGON, BEAR, AND TROLL.  TREASURES SPECIAL FOR TROLL.

L9170:
  if (_Toting_(ROD2) && OBJ eq ROD && ~_Toting_(ROD)) then OBJ = ROD2
  if (~_Toting_(OBJ)) then goto, L2011
  if (OBJ ge 50 && OBJ le MAXTRS && _At_(TROLL)) then goto, L9178
  if (OBJ eq FOOD && _Here_(BEAR)) then goto, L9177
  if (OBJ ne AXE) then goto, L9020
  for I = 1,5 do begin
    ;  NEEDN'T CHECK DFLAG IF AXE IS HERE.
    if (DLOC[I] eq LOC) then goto, L9172
  endfor
  SPK = 152
  if (_At_(DRAGON) && PROP[DRAGON] eq 0) then goto, L9175
  SPK = 158
  if (_At_(TROLL)) then goto, L9175
  if (_Here_(BEAR) && PROP[BEAR] eq 0) then goto, L9176
  OBJ = 0
  goto, L9120

L9172:
  SPK = 48
  if (_RND_(3) eq 0) then goto, L9175
  DSEEN[I] = FALSE
  DLOC[I] = 0
  SPK = 47
  DKILL++
  if (DKILL eq 1) then SPK = 149
L9175:
  _RSPEAK_, SPK
  _Drop_, AXE, LOC
  K = NULL
  goto, L8

;  THIS'LL TEACH HIM TO THROW THE AXE AT THE BEAR!

L9176:
  SPK = 164
  _Drop_, AXE, LOC
  FIXED[AXE] = -1
  PROP[AXE] = 1
  _JUGGLE_, BEAR
  goto, L2011

;  BUT THROWING FOOD IS ANOTHER STORY.

L9177:
  OBJ = BEAR
  goto, L9210

; SNARF A TREASURE FOR THE TROLL.

L9178:
  SPK = 159
  _Drop_, OBJ, 0
  _Move_, TROLL, 0
  _Move_, TROLL+100, 0
  _Drop_, TROLL2, PLAC[TROLL]
  _Drop_, TROLL2+100, FIXD[TROLL]
  _JUGGLE_, CHASM
  goto, L2011

;  QUIT.  INTRANSITIVE ONLY.  VERIFY INTENT AND EXIT IF THAT'S WHAT HE WANTS.

L8180:
  _RSPEAK_, 22
  if (~DEBUG && WIDBASE gt 0) then return, 5
CMD5:
  GAVEUP = _YES_(22,54,54)
L8185:
  if (GAVEUP) then goto, L20000
  goto, L2012

;  FIND.  MIGHT BE CARRYING IT, OR IT MIGHT BE HERE.  ELSE GIVE CAVEAT.

L9190:
  if (_At_(OBJ) || (_Liq_() eq OBJ && _At_(BOTTLE)) $
    || K eq _Liqloc_()) then SPK = 94
  for I = 1,5 do begin
    if (DLOC[I] eq LOC && DFLAG ge 2 && OBJ eq DWARF) then SPK = 94
  endfor
  if (CLOSED) then SPK = 138
  if (_Toting_(OBJ)) then SPK = 24
  goto, L2011

;  INVENTORY.  IF OBJECT, TREAT SAME AS FIND.  ELSE REPORT ON CURRENT BURDEN.

L8200:
  SPK = 98
  for I = 1,100 do begin
    if (I eq BEAR || ~_TOTING_(I)) then continue
    if (SPK eq 98) then _RSPEAK_, 99
    _PSPEAK_, I, -1
    SPK = 0
  endfor
  if (_Toting_(BEAR)) then SPK = 141
  goto, L2011

;  FEED.  IF BIRD, NO SEED.  SNAKE, DRAGON, TROLL: QUIP.  IF DWARF, MAKE HIM
;  MAD.  BEAR, SPECIAL.

L9210:
  if (OBJ ne BIRD) then goto, L9212
  SPK = 100
  goto, L2011

L9212:
  if (OBJ ne SNAKE && OBJ ne DRAGON && OBJ ne TROLL) then goto, L9213
  SPK = 102
  if (OBJ eq DRAGON && PROP[DRAGON] ne 0) then SPK = 110
  if (OBJ eq TROLL) then SPK = 182
  if (OBJ ne SNAKE || CLOSED || ~_Here_(BIRD)) then goto, L2011
  SPK = 101
  _DSTROY_, BIRD
  PROP[BIRD] = 0
  TALLY2++
  goto, L2011

L9213:
  if (OBJ ne DWARF) then goto, L9214
  if (~_Here_(FOOD)) then goto, L2011
  SPK = 103
  DFLAG++
  goto, L2011

L9214:
  if (OBJ ne BEAR) then goto, L9215
  if (PROP[BEAR] eq 0) then SPK = 102
  if (PROP[BEAR] eq 3) then SPK = 110
  if (~_Here_(FOOD)) then goto, L2011
  _DSTROY_, FOOD
  PROP[BEAR] = 1
  FIXED[AXE] = 0
  PROP[AXE] = 0
  SPK = 168
  goto, L2011

L9215:
  SPK = 14
  goto, L2011

;  FILL.  BOTTLE MUST BE EMPTY, AND SOME LIQUID AVAILABLE.  (VASE IS NASTY.)

L9220:
  if (OBJ eq VASE) then goto, L9222
  if (OBJ ne 0 && OBJ ne BOTTLE) then goto, L2011
  if (OBJ eq 0 && ~_Here_(BOTTLE)) then goto, L8000
  SPK = 107
  if (_Liqloc_() eq 0) then SPK = 106
  if (_Liq_() ne 0) then SPK = 105
  if (SPK ne 107) then goto, L2011
  PROP[BOTTLE] = (CONDITIONS[LOC] mod 4)/2*2
  K = _Liq_()
  if (_Toting_(BOTTLE)) then PLACE[K] = -1
  if (K eq OIL) then SPK = 108
  goto, L2011

L9222:
  SPK = 29
  if (_Liqloc_() eq 0) then SPK = 144
  if (_Liqloc_() eq 0 || ~_Toting_(VASE)) then goto, L2011
  _RSPEAK_, 145
  PROP[VASE] = 2
  FIXED[VASE] = -1
  goto, L9024

;  BLAST.  NO EFFECT UNLESS YOU'VE GOT DYNAMITE, WHICH IS A NEAT TRICK!

L9230:
  if (PROP[ROD2] lt 0 || ~CLOSED) then goto, L2011
  BONUS = 133
  if (LOC eq 115) then BONUS = 134
  if (_Here_(ROD2)) then BONUS = 135
  _RSPEAK_, BONUS
  goto, L20000

;  SCORE.  GO TO SCORING SECTION, WHICH WILL RETURN TO 8241 IF SCORNG IS TRUE.

L8240:
  SCORNG = TRUE
  goto, L20000

L8241:
  SCORNG = FALSE
  _Speak_, 'If you were to quit now, you would score ' + $
    Strtrim(SCORE,2) + ' out of a possible ' + Strtrim(MXSCOR,2) + '.'
  _RSPEAK_, 143
  if (~DEBUG && WIDBASE gt 0) then return, 6
CMD6:
  GAVEUP = _YES_(143,54,54)
  goto, L8185

;  FEE FIE FOE FOO (AND FUM).  ADVANCE TO NEXT STATE IF GIVEN IN PROPER ORDER.
;  LOOK UP WD1 IN SECTION 3 OF VOCAB TO DETERMINE WHICH WORD WE'VE GOT.  LAST
;  WORD ZIPS THE EGGS BACK TO THE GIANT ROOM (UNLESS ALREADY THERE).

L8250:
  _Vocab_, WD1, WD1A, 3, K
  SPK = 42
  if (FOOBAR eq 1-K) then goto, L8252
  if (FOOBAR ne 0) then SPK = 151
  goto, L2011

L8252:
  FOOBAR = K
  if (K ne 4) then goto, L2009
  FOOBAR = 0
  if (PLACE[EGGS] eq PLAC[EGGS] $
        || (_Toting_(EGGS) && LOC eq PLAC[EGGS])) then goto, L2011

;  BRING BACK TROLL IF WE STEAL THE EGGS BACK FROM HIM BEFORE CROSSING.

  if (PLACE[EGGS] eq 0 && PLACE[TROLL] eq 0 && PROP[TROLL] eq 0) then PROP[TROLL] = 1
  K = 2
  if (_Here_(EGGS)) then K = 1
  if (LOC eq PLAC[EGGS]) then K = 0
  _Move_, EGGS, PLAC[EGGS]
  _PSpeak_, EGGS, K
  goto, L2012

;  BRIEF.  INTRANSITIVE ONLY.  SUPPRESS LONG DESCRIPTIONS AFTER FIRST TIME.

L8260:
  SPK = 156
  ABBNUM = 10000
  DETAIL = 3
  goto, L2011

;  READ.  MAGAZINES IN DWARVISH, MESSAGE WE'VE SEEN, AND . . . OYSTER?

L8270:
  if (_Here_(MAGZIN)) then OBJ = MAGZIN
  if (_Here_(TABLET)) then OBJ = OBJ*100+TABLET
  if (_Here_(MESSAG)) then OBJ = OBJ*100+MESSAG
  if (CLOSED && _Toting_(OYSTER)) then OBJ = OYSTER
  if (OBJ gt 100 || OBJ eq 0 || _Dark_()) then goto, L8000

L9270:
  if (_Dark_()) then goto, L5190
  if (OBJ eq MAGZIN) then SPK = 190
  if (OBJ eq TABLET) then SPK = 196
  if (OBJ eq MESSAG) then SPK = 191
  if (OBJ eq OYSTER && HINTED[2] && _Toting_(OYSTER)) then SPK = 194
  if (OBJ ne OYSTER || HINTED[2] || ~_Toting_(OYSTER) $
        || ~CLOSED) then goto, L2011
  _RSPEAK_, 192
  if (~DEBUG && WIDBASE gt 0) then return, 7
CMD7:
  HINTED[2] = _YES_(192,193,54)
  goto, L2012

;  BREAK.  ONLY WORKS FOR MIRROR IN REPOSITORY AND, OF COURSE, THE VASE.

L9280:
  if (OBJ eq MIRROR) then SPK = 148
  if (OBJ eq VASE && PROP[VASE] eq 0) then goto, L9282
  if (OBJ ne MIRROR || ~CLOSED) then goto, L2011
  _RSPEAK_, 197
  goto, L19000

L9282:
  SPK = 198
  if (_Toting_(VASE)) then _Drop_, VASE, LOC
  PROP[VASE] = 2
  FIXED[VASE] = -1
  goto, L2011

;  WAKE.  ONLY USE IS TO DISTURB THE DWARVES.

L9290:
  if (OBJ ne DWARF || ~CLOSED) then goto, L2011
  _RSPEAK_, 199
  goto, L19000

; SUSPEND.  SAVE THE WORLD.

L8300:
  _SAVEGM_, TRUE
  goto, L2012

; HOURS.  JUST A JOKE.

L8310:
  _RSPEAK_, 201
  goto, L2012

; RESUME.  RESTORE THE WORLD.

L8320:
  _RSTRGM_, TRUE
  goto, L2012

;  HINTS

;  COME HERE IF HE'S BEEN LONG ENOUGH AT REQUIRED LOC[S] FOR SOME UNUSED HINT.
;  HINT NUMBER IS IN VARIABLE "HINT".  BRANCH TO QUICK TEST FOR ADDITIONAL
;  CONDITIONS, THEN COME BACK TO DO NEAT STUFF.  goto, L40010 IF CONDITIONS ARE
;  MET AND WE WANT TO OFFER THE HINT.  goto, L40020 TO CLEAR HINTLC BACK TO ZERO,
;  40030 TO TAKE NO ACTION YET.

L40000:
  case (HINT-3) of
  1: goto, L40400 ; CAVE
  2: goto, L40500 ; BIRD
  3: goto, L40600 ; SNAKE
  4: goto, L40700 ; MAZE
  5: goto, L40800 ; DARK
  6: goto, L40900 ; WITT
  else: Message, 'HINT NUMBER EXCEEDS LIST'
  endcase

L40010:
  HINTLC[HINT] = 0
  _RSPEAK_, HINTS[HINT,3]
  if (~DEBUG && WIDBASE gt 0) then return, 8
CMD8:
  if (~_YES_(HINTS[HINT,3],0,54)) then goto, L2602
  _Speak_, 'I am prepared to give you a hint, but it will cost you ' + $
    Strtrim(HINTS[HINT,2],2) + ' points.'
  _RSPEAK_, 175
  if (~DEBUG && WIDBASE gt 0) then return, 9
CMD9:
  HINTED[HINT] = _YES_(175,HINTS[HINT,4],54)
  if (HINTED[HINT] && LIMIT gt 30) then LIMIT += 30*HINTS[HINT,2]
L40020:
  HINTLC[HINT] = 0
L40030:
  goto, L2602

;  NOW FOR THE QUICK TESTS.  SEE DATABASE DESCRIPTION FOR ONE-LINE NOTES.

L40400:
  if (PROP[GRATE] eq 0 && ~_Here_(KEYS)) then goto, L40010
  goto, L40020

L40500:
  if (_Here_(BIRD) && _Toting_(ROD) && OBJ eq BIRD) then goto, L40010
  goto, L40030

L40600:
  if (_Here_(SNAKE) && ~_Here_(BIRD)) then goto, L40010
  goto, L40020

L40700:
  if (ATLOC[LOC] eq 0 && ATLOC[OLDLOC] eq 0 $
        && ATLOC[OLDLC2] eq 0 && HOLDNG gt 1) then goto, L40010
  goto, L40020

L40800:
  if (PROP[EMRALD] ne -1 && PROP[PYRAM] eq -1) then goto, L40010
  goto, L40020

L40900: goto, L40010

;  CAVE CLOSING AND SCORING


;  THESE SECTIONS HANDLE THE CLOSING OF THE CAVE.  THE CAVE CLOSES "CLOCK1"
;  TURNS AFTER THE LAST TREASURE HAS BEEN LOCATED (INCLUDING THE PIRATE'S
;  CHEST, WHICH MAY OF COURSE NEVER SHOW UP).  NOTE THAT THE TREASURES NEED NOT
;  HAVE BEEN TAKEN YET, JUST LOCATED.  HENCE CLOCK1 MUST BE LARGE ENOUGH TO GET
;  OUT OF THE CAVE (IT ONLY TICKS WHILE INSIDE THE CAVE).  WHEN IT HITS ZERO,
;  WE BRANCH TO 10000 TO START CLOSING THE CAVE, AND THEN SIT BACK AND WAIT FOR
;  HIM TO TRY TO GET OUT.  IF HE DOESN'T WITHIN CLOCK2 TURNS, WE CLOSE THE
;  CAVE; IF HE DOES TRY, WE ASSUME HE PANICS, AND GIVE HIM A FEW ADDITIONAL
;  TURNS TO GET FRANTIC BEFORE WE CLOSE.  WHEN CLOCK2 HITS ZERO, WE BRANCH TO
;  11000 TO TRANSPORT HIM INTO THE FINAL PUZZLE.  NOTE THAT THE PUZZLE DEPENDS
;  UPON ALL SORTS OF RANDOM THINGS.  FOR INSTANCE, THERE MUST BE NO WATER OR
;  OIL, SINCE THERE ARE BEANSTALKS WHICH WE DON'T WANT TO BE ABLE TO WATER,
;  SINCE THE CODE CAN'T HANDLE IT.  ALSO, WE CAN HAVE NO KEYS, SINCE THERE IS A
;  GRATE (HAVING MOVED THE FIXED OBJECT!) THERE SEPARATING HIM FROM ALL THE
;  TREASURES.  MOST OF THESE PROBLEMS ARISE FROM THE USE OF NEGATIVE PROP
;  NUMBERS TO SUPPRESS THE OBJECT DESCRIPTIONS UNTIL HE'S ACTUALLY MOVED THE
;  OBJECTS.

;  WHEN THE FIRST WARNING COMES, WE LOCK THE GRATE, DESTROY THE BRIDGE, KILL
;  ALL THE DWARVES (AND THE PIRATE), REMOVE THE TROLL AND BEAR (UNLESS DEAD),
;  AND SET "CLOSNG" TO TRUE.  LEAVE THE DRAGON; TOO MUCH TROUBLE TO MOVE IT.
;  FROM NOW UNTIL CLOCK2 RUNS OUT, HE CANNOT UNLOCK THE GRATE, MOVE TO ANY
;  LOCATION OUTSIDE THE CAVE (LOC<9), OR CREATE THE BRIDGE.  NOR CAN HE BE
;  RESURRECTED IF HE DIES.  NOTE THAT THE SNAKE IS ALREADY GONE, SINCE HE GOT
;  TO THE TREASURE ACCESSIBLE ONLY VIA THE HALL OF THE MT. KING.  ALSO, HE'S
;  BEEN IN GIANT ROOM (TO GET EGGS), SO WE CAN REFER TO IT.  ALSO ALSO, HE'S
;  GOTTEN THE PEARL, SO WE KNOW THE BIVALVE IS AN OYSTER.  *AND*, THE DWARVES
;  MUST HAVE BEEN ACTIVATED, SINCE WE'VE FOUND CHEST.

L10000:
  PROP[GRATE] = 0
  PROP[FISSUR] = 0
  for I = 1,6 do begin
    DSEEN[I] = FALSE
    DLOC[I] = 0
  endfor
  _Move_, TROLL, 0
  _Move_, TROLL+100, 0
  _Move_, TROLL2, PLAC[TROLL]
  _Move_, TROLL2+100, FIXD[TROLL]
  _JUGGLE_, CHASM
  if (PROP[BEAR] ne 3) then _DSTROY_, BEAR
  PROP[CHAIN] = 0
  FIXED[CHAIN] = 0
  PROP[AXE] = 0
  FIXED[AXE] = 0
  _RSPEAK_, 129
  CLOCK1 = -1
  CLOSNG = TRUE
  goto, L19999

;  ONCE HE'S PANICKED, AND CLOCK2 HAS RUN OUT, WE COME HERE TO SET UP THE
;  STORAGE ROOM.  THE ROOM HAS TWO LOCS, HARDWIRED AS 115 [NE] AND 116 [SW].
;  AT THE NE END, WE PLACE EMPTY BOTTLES, A NURSERY OF PLANTS, A BED OF
;  OYSTERS, A PILE OF LAMPS, RODS WITH STARS, SLEEPING DWARVES, AND HIM.  AND
;  THE SW END WE PLACE GRATE OVER TREASURES, SNAKE PIT, COVEY OF CAGED BIRDS,
;  MORE RODS, AND PILLOWS.  A MIRROR STRETCHES ACROSS ONE WALL.  MANY OF THE
;  OBJECTS COME FROM KNOWN LOCATIONS AND/OR STATES (E.G. THE SNAKE IS KNOWN TO
;  HAVE BEEN DESTROYED AND NEEDN'T BE CARRIED AWAY FROM ITS OLD "PLACE"),
;  MAKING THE VARIOUS OBJECTS BE HANDLED DIFFERENTLY.  WE ALSO DROP ALL OTHER
;  OBJECTS HE MIGHT BE CARRYING (LEST HE HAVE SOME WHICH COULD CAUSE TROUBLE,
;  SUCH AS THE KEYS).  WE DESCRIBE THE FLASH OF LIGHT AND TRUNDLE BACK.

L11000:
  PROP[BOTTLE] = _PUT_(BOTTLE,115,1)
  PROP[PLANT] = _PUT_(PLANT,115,0)
  PROP[OYSTER] = _PUT_(OYSTER,115,0)
  PROP[LAMP] = _PUT_(LAMP,115,0)
  PROP[ROD] = _PUT_(ROD,115,0)
  PROP[DWARF] = _PUT_(DWARF,115,0)
  LOC = 115
  OLDLOC = 115
  NEWLOC = 115

;  LEAVE THE GRATE WITH NORMAL (NON-NEGATIVE PROPERTY).

  I = _PUT_(GRATE,116,0)
  PROP[SNAKE] = _PUT_(SNAKE,116,1)
  PROP[BIRD] = _PUT_(BIRD,116,1)
  PROP[CAGE] = _PUT_(CAGE,116,0)
  PROP[ROD2] = _PUT_(ROD2,116,0)
  PROP[PILLOW] = _PUT_(PILLOW,116,0)

  PROP[MIRROR] = _PUT_(MIRROR,115,0)
  FIXED[MIRROR] = 116

  for I = 1,100 do begin
    if (_Toting_(I)) then _DSTROY_, I
  endfor

  _RSPEAK_, 132
  CLOSED = TRUE
  goto, L2

;  ANOTHER WAY WE CAN FORCE AN END TO THINGS IS BY HAVING THE LAMP GIVE OUT.
;  WHEN IT GETS CLOSE, WE COME HERE TO WARN HIM.  WE GO TO 12000 IF THE LAMP
;  AND FRESH BATTERIES ARE HERE, IN WHICH CASE WE REPLACE THE BATTERIES AND
;  CONTINUE.  12200 IS FOR OTHER CASES OF LAMP DYING.  12400 IS WHEN IT GOES
;  OUT, AND 12600 IS IF HE'S WANDERED OUTSIDE AND THE LAMP IS USED UP, IN WHICH
;  CASE WE FORCE HIM TO GIVE UP.

L12000:
  _RSPEAK_, 188
  PROP[BATTER] = 1
  if (_Toting_(BATTER)) then _Drop_, BATTER, LOC
  LIMIT = LIMIT + 2500
  LMWARN = FALSE
  goto, L19999

L12200:
  if (LMWARN || ~_Here_(LAMP)) then goto, L19999
  LMWARN = TRUE
  SPK = 187
  if (PLACE[BATTER] eq 0) then SPK = 183
  if (PROP[BATTER] eq 1) then SPK = 189
  _RSPEAK_, SPK
  goto, L19999

L12400:
  LIMIT = -1
  PROP[LAMP] = 0
  if (_Here_(LAMP)) then _RSPEAK_, 184
  goto, L19999

L12600:
  _RSPEAK_, 185
  GAVEUP = TRUE
  goto, L20000


;  OH DEAR, HE'S DISTURBED THE DWARVES.

L19000:
  _RSPEAK_, 136

;  EXIT CODE.  WILL EVENTUALLY INCLUDE SCORING.  FOR NOW, HOWEVER, ...

;  THE PRESENT SCORING ALGORITHM IS AS FOLLOWS:
;     OBJECTIVE:      POINTS:    PRESENT TOTAL POSSIBLE:
;  GETTING WELL INTO CAVE   25        25
;  EACH TREASURE < CHEST    12        60
;  TREASURE CHEST ITSELF    14        14
;  EACH TREASURE > CHEST    16       144
;  SURVIVING     (MAX-NUM)*10     30
;  NOT QUITTING      4         4
;  REACHING "CLOSNG"    25        25
;  "CLOSED": QUIT/KILLED    10
;    KLUTZED    25
;    WRONG WAY  30
;    SUCCESS    45        45
;  CAME TO WITT'S END    1         1
;  ROUND OUT THE TOTAL   2         2
;               TOTAL:   350
;  (POINTS CAN ALSO BE DEDUCTED FOR USING HINTS.)

L20000:
  SCORE = 0
  MXSCOR = 0

;  FIRST TALLY UP THE TREASURES.  MUST BE IN BUILDING AND NOT BROKEN.
;  GIVE THE POOR GUY 2 POINTS JUST FOR FINDING EACH TREASURE.

  for I = 50,MAXTRS do begin
    if (PTEXT[I,0] eq '') then continue
    K = 12
    if (I eq CHEST) then K = 14
    if (I gt CHEST) then K = 16
    if (PROP[I] ge 0) then SCORE = SCORE+2
    if (PLACE[I] eq 3 && PROP[I] eq 0) then SCORE = SCORE+K-2
    MXSCOR = MXSCOR+K
  endfor

;  NOW LOOK AT HOW HE FINISHED AND HOW FAR HE GOT.  MAXDIE AND NUMDIE TELL US
;  HOW WELL HE SURVIVED.  GAVEUP SAYS WHETHER HE EXITED VIA QUIT.  DFLAG WILL
;  TELL US IF HE EVER GOT SUITABLY DEEP INTO THE CAVE.  CLOSNG STILL INDICATES
;  WHETHER HE REACHED THE ENDGAME.  AND IF HE GOT AS FAR AS "CAVE CLOSED"
;  (INDICATED BY "CLOSED"), THEN BONUS IS ZERO FOR MUNDANE EXITS OR 133, 134,
;  135 IF HE BLEW IT (SO TO SPEAK).

  SCORE = SCORE+(MAXDIE-NUMDIE)*10
  MXSCOR = MXSCOR+MAXDIE*10
  if (~(SCORNG || GAVEUP)) then SCORE = SCORE+4
  MXSCOR = MXSCOR+4
  if (DFLAG ne 0) then SCORE = SCORE+25
  MXSCOR = MXSCOR+25
  if (CLOSNG) then SCORE = SCORE+25
  MXSCOR = MXSCOR+25
  if (~CLOSED) then goto, L20020
  if (BONUS eq 0) then SCORE = SCORE+10
  if (BONUS eq 135) then SCORE = SCORE+25
  if (BONUS eq 134) then SCORE = SCORE+30
  if (BONUS eq 133) then SCORE = SCORE+45
L20020:
  MXSCOR = MXSCOR+45

;  DID HE COME TO WITT'S END AS HE SHOULD?

  if (PLACE[MAGZIN] eq 108) then SCORE = SCORE+1
  MXSCOR = MXSCOR+1

;  ROUND IT OFF.

  SCORE = SCORE+2
  MXSCOR = MXSCOR+2

;  DEDUCT POINTS FOR HINTS.  HINTS < 4 ARE SPECIAL; SEE DATABASE DESCRIPTION.

  for I = 1,HNTMAX do begin
    if (HINTED[I]) then SCORE = SCORE-HINTS[I,2]
  endfor

;  RETURN TO SCORE COMMAND IF THAT'S WHERE WE CAME FROM.

  if (SCORNG) then goto, L8241

;  THAT SHOULD BE GOOD ENOUGH.  LET'S TELL HIM ALL ABOUT IT.

  str = 'You scored ' + Strtrim(SCORE,2) + ' out of a possible ' + $
    Strtrim(MXSCOR,2) + ', using ' + Strtrim(TURNS,2) + ' turns.'
  _Speak_, str

  for I = 1,CLSSES do begin
    if (CVAL[I] ge SCORE) then goto, L20210
  endfor
  _Speak_, 'You just went off my scale!!'
  goto, L25000

L20210:
  _Speak_, CTEXT[I]
  if (I eq CLSSES-1) then goto, L20220
  K = CVAL[I]+1-SCORE
  _Speak_, 'To achieve the next higher rating, you need ' + Strtrim(K,2) + $
    ' more point' + ((K eq 1) ? '.' : 's.')
  goto, L25000

L20220:
  _Speak_, 'To achieve the next higher rating would be a neat trick!'
  _Speak_, 'Congratulations!!'

L25000:
  return, -1   ; Allow restart

end

;--------------------------------------------------------------------
pro IDL_Adventure_Event, event
  compile_opt idl2, hidden
  common TXTCOM
  if (TAG_NAMES(event, /STRUCTURE) eq 'WIDGET_BASE') then begin
    Widget_Control, WIDBASE, GET_UVALUE=border
    xsize = event.x - border[0]
    ysize = event.y - border[1]
    Widget_Control, WIDTEXT, SCR_XSIZE=xsize, SCR_YSIZE=ysize
    Widget_Control, WIDINPUT, SCR_XSIZE=xsize
  endif
end

;--------------------------------------------------------------------
pro IDL_Adventure_TextEvent, event
  compile_opt idl2, hidden
  common TXTCOM
  Widget_Control, event.id, GET_VALUE=COMMAND
  if (COMMAND eq '') then begin
    ; Ignore null strings unless the game is over,
    ; in which case start a new game.
    if (CMD ne -1) then return
    ; Start a new game.
    IDL_Adventure_Init
    CMD = -1
  endif
  Widget_Control, event.id, SET_VALUE=''
  _Speak_, '>>> ' + COMMAND
  currCmd = CMD
  CMD = IDL_Adventure_Main(currCmd)
end

;--------------------------------------------------------------------
pro IDL_Adventure, DEBUG=debugIn, COMMAND_LINE=commandLine

  compile_opt idl2
  common VERSN
  common TXTCOM

  DEBUG = Keyword_Set(debugIn)
  if (LMGR(/RUNTIME) || LMGR(/VM)) then commandLine = 0b
  WIDBASE = 0L
  WIDTEXT = 0L
  WIDINPUT = 0L
  NLINES = 0L

  cd, FILE_DIRNAME(Routine_Filepath('IDL_Adventure'), /MARK)
  IDL_Adventure_Init

  if (~Keyword_Set(commandLine)) then begin
    font = 'Courier*Bold'
    WIDBASE = Widget_Base(/COLUMN, TITLE='IDL Adventure', /TLB_SIZE_EVENTS)
    WIDTEXT = Widget_Text(WIDBASE, EDITABLE=0, FONT=font, $
      /SCROLL, XSIZE=80, YSIZE=40, VALUE=STRARR(20))
    NLINES = 20L
    WIDINPUT = Widget_Text(WIDBASE, EDITABLE=1, FONT=font, $
      EVENT_PRO='IDL_Adventure_TextEvent')
    Widget_control, WIDBASE, /REALIZE
    geom1 = Widget_Info(WIDBASE, /GEOM)
    geom2 = Widget_Info(WIDTEXT, /GEOM)
    geom3 = Widget_Info(WIDINPUT, /GEOM)
    xpad = geom1.xsize - (geom2.xsize > geom2.scr_xsize)
    ypad = geom1.ysize - (geom2.ysize > geom2.scr_ysize)
    Widget_control, WIDBASE, SET_UVALUE=[xpad,ypad]
    Xmanager, 'IDL_Adventure', WIDBASE, /NO_BLOCK
  endif

  CMD = IDL_Adventure_Main()

end


