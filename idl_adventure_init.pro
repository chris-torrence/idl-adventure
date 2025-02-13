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

;      IMPLICIT INTEGER*2 (A-Z)
;      LOGICAL DSEEN,HINTED
;      LOGICAL BITSET,LMWARN,CLOSNG,PANIC,
;               CLOSED,GAVEUP,SCORNG,bitsy

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
;  START TOTING AN OBJECT, REMOVING IT FROM THE LIST OF THINGS AT ITS FORMER
;  LOCATION.  INCR HOLDNG UNLESS IT WAS ALREADY BEING TOTED.  IF OBJECT>100
;  (MOVING "FIXED" SECOND LOC), DON'T CHANGE PLACE OR HOLDNG.
pro _Carry_, OBJECT, WHERE

  compile_opt idl2, hidden

  common PLACOM
  
  if (OBJECT gt 100) then goto, L5
  if (PLACE[OBJECT] eq -1) then Return
  PLACE[OBJECT] = -1
  HOLDNG++
L5:
  if (ATLOC[Where] ne OBJECT) then goto, L6
  ATLOC[WHERE] = LINK[OBJECT]
  Return
L6:
  TEMP = ATLOC[Where]
L7:
  if (LINK[TEMP] eq OBJECT) then goto, L8
  TEMP = LINK[TEMP]
  goto, L7
L8:
  LINK[TEMP] = LINK[OBJECT]
end


;--------------------------------------------------------------------
;  PLACE AN OBJECT AT A GIVEN LOC, PREFIXING IT ONTO THE ATLOC LIST.  DECR
;  HOLDNG IF THE OBJECT WAS BEING TOTED.
pro _Drop_, OBJECT, WHERE

  compile_opt idl2, hidden

  common PLACOM
  
  if (OBJECT gt 100) then begin
    FIXED[OBJECT - 100] = WHERE
  endif else begin
    if (PLACE[OBJECT] eq -1) then HOLDNG--
    PLACE[OBJECT] = WHERE
  endelse
  if (WHERE le 0) then Return
if object eq 0 then stop
  LINK[OBJECT] = ATLOC[Where]
  ATLOC[WHERE] = OBJECT
end


;--------------------------------------------------------------------
;  PLACE ANY OBJECT ANYWHERE BY PICKING IT UP AND DROPPING IT.  MAY ALREADY BE
;  TOTING, IN WHICH CASE THE CARRY IS A NO-OP.  MUSTN'T PICK UP OBJECTS WHICH
;  ARE NOT AT ANY LOC, SINCE CARRY WANTS TO REMOVE OBJECTS FROM ATLOC CHAINS.
pro _Move_, OBJECT, WHERE

  compile_opt idl2, hidden

  common PLACOM

  if (OBJECT gt 100) then begin
    FROM = FIXED[OBJECT-100]
  endif else begin
    FROM = PLACE[OBJECT]
  endelse

  if (FROM gt 0 && FROM le 300) then _Carry_, OBJECT, FROM
  _DROP_, OBJECT,WHERE
end

;--------------------------------------------------------------------
;  PERMANENTLY ELIMINATE "OBJECT" BY MOVING TO A NON-EXISTENT LOCATION.
pro _Dstroy_, OBJECT
  compile_opt idl2, hidden
  _Move_, OBJECT, 0
end

;--------------------------------------------------------------------
;  PUT IS THE SAME AS MOVE, EXCEPT IT RETURNS A VALUE USED TO SET UP THE
;  NEGATED PROP VALUES FOR THE REPOSITORY OBJECTS.
function _Put_, OBJECT, WHERE, PVAL
  compile_opt idl2, hidden
  _Move_, OBJECT, WHERE
  Return, (-1)-PVAL
end

;--------------------------------------------------------------------
;  JUGGLE AN OBJECT BY PICKING IT UP AND PUTTING IT DOWN AGAIN, THE PURPOSE
;  BEING TO GET THE OBJECT TO THE FRONT OF THE CHAIN OF THINGS AT ITS LOC.
pro _Juggle_, OBJECT

  compile_opt idl2, hidden
  common PLACOM
  
  I = PLACE[OBJECT]
  J = FIXED[OBJECT]
  _MOVE_, OBJECT, I
  _MOVE_, OBJECT+100, J
end

;--------------------------------------------------------------------
;  PRINT THE MESSAGE IN RECORD N OF THE RANDOM ACCESS MESSAGE FILE.
pro _Speak_, msg
  compile_opt idl2, hidden
  common TXTCOM
  
  if (msg eq '>$<') then Return
  
  msg1 = Strtok(msg, String(10b), /EXTRACT)

  if (WIDBASE ne 0) then begin
    if (Widget_Info(WIDTEXT,/VALID)) then begin
      Widget_Control, WIDTEXT, /APPEND, SET_VALUE=msg1
      NLINES += N_Elements(msg1)
      geom = Widget_Info(WIDTEXT,/GEOM)
      Widget_Control, WIDTEXT, SET_TEXT_TOP_LINE=NLINES-geom.ysize+2
    endif
  endif else begin
    Print, msg1, FORMAT='(A)'
  endelse
end

;--------------------------------------------------------------------
;  FIND THE SKIP+1ST MESSAGE FOR OBJECT MSG AND PRINT IT.
;  MSG SHOULD BE THE INDEX OF
;  THE OBJECT.  (INVEN+N+1 MESSAGE IS PROP = N MESSAGE).
pro _Pspeak_, MSG, SKIP
  compile_opt idl2, hidden
  common PTXCOM
  _Speak_, PTEXT[MSG, SKIP + 1]
end

;--------------------------------------------------------------------
;  PRINT THE I-TH "RANDOM" MESSAGE (SECTION 6 OF DATABASE).
pro _Rspeak_, I
  compile_opt idl2, hidden
  common TXTCOM
  if (I ne 0) then _Speak_, RTEXT[I]
end

;--------------------------------------------------------------------
;  GET A COMMAND FROM THE ADVENTURER.  SNARF OUT THE FIRST WORD, PAD IT WITH
;  BLANKS, AND RETURN IT IN WORD1 AND WORD1A.
;  CHARS 5  AND 6 ARE RETURNED IN WORD1X, IN
;  CASE WE NEED TO PRINT OUT THE WHOLE WORD IN AN ERROR MESSAGE.  ANY NUMBER OF
;  BLANKS MAY FOLLOW THE WORD.  IF A SECOND WORD APPEARS, IT IS RETURNED IN
;  WORD2 AND WORD2A (CHARS 5 AND 68 IN WORD2X), ELSE WORD2 IS SET TO ZERO.
pro _Getin_, word1, word1a, word1x, word2, word2a, word2x

  compile_opt idl2, hidden

  common VERSN
  common VOCCOM
  common TXTCOM
  
  str = ''
  if (DEBUG) then begin
    TABNDX = Min(Where(KTAB eq -1)) - 1
    idx = 0
    idx = Fix(randomu(s,1)*TABNDX) + 1
    str = ATAB[idx] + A2TAB[idx]
    if randomu(s,1) le 0.5 then begin
tryAgain:
      idx = Fix(randomu(s,1)*TABNDX) + 1
      str2 = ATAB[idx] + A2TAB[idx]
      if (str2 eq 'SCOR' || str2 eq 'QUIT') then goto, tryAgain
      str += ' ' + str2
    endif
    _Speak_, str
  endif else begin
    if (WIDBASE ne 0) then begin
      str = COMMAND
      COMMAND = ''
    endif else begin
      Read, str, PROMPT='>>> '
    endelse
  endelse
  str = Strupcase(str)
  words = Strtok(str, ' ', /EXTRACT)
  len = Strlen(words[0])
  word1 = Strmid(words[0], 0, 2)
  word1a = Strmid(words[0], 2, 2)
  word1x = Strmid(words[0], 4, 2)
  if (N_Elements(words) ge 2) then begin
    word2 = Strmid(words[1], 0, 2)
    word2a = Strmid(words[1], 2, 2)
    word2x = Strmid(words[1], 4, 2)
  endif else begin
    word2 = ''
    word2a = ''
    word2x = ''
  endelse
end

;--------------------------------------------------------------------
pro _Vocab_,ID1,ID2,INIT,V

;  LOOK UP ID1:ID2 IN THE VOCABULARY (ATAB AND A2TAB)
;  AND RETURN ITS "DEFINITION" [KTAB], OR
;  -1 IF NOT FOUND.  IF INIT IS POSITIVE, THIS IS AN INIT CALL SETTING
;  UP A KEYWORD VARIABLE, AND NOT FINDING IT CONSTITUTES A BUG.  IT ALSO MEANS
;  THAT ONLY KTAB VALUES WHICH TAKEN OVER 1000 EQUAL INIT MAY BE CONSIDERED.
;  (THUS "STEPS", WHICH IS A MOTION VERB AS WELL AS AN OBJECT, MAY BE LOCATED
;  AS AN OBJECT.)  AND IT ALSO MEANS THE KTAB VALUE IS TAKEN MOD 1000.

  compile_opt idl2, hidden
  common VOCCOM, KTAB,ATAB,A2TAB,TABSIZ
  
  for i = 1,TABSIZ do begin
    if (KTAB[I] eq -1) then goto, L2
    if (INIT ge 0 && KTAB[I]/1000 ne INIT) then continue
    if (ATAB[I] eq ID1 && A2TAB[I] eq ID2) then goto, L3
  endfor
;    write(*,*)'id1,id2,init,v,tabsiz = ',id1,id2,init,v,tabsiz
;    write(*,999) then id1,id2
;    L999:   format(' id1//id2  = ',2a2)
;    CALL BUG(int2[21])
    
L2:     V = -1
    if (INIT lt 0) then Return
    Message, 'REQUIRED VOCABULARY WORD NOT FOUND'
    
L3:
  V = KTAB[I]
  if (INIT ge 0) then V = V mod 1000

end

;--------------------------------------------------------------------
;  PRINT MESSAGE X, WAIT FOR YES/NO ANSWER.  IF YES, PRINT Y AND LEAVE YEA
;  TRUE; IF NO, PRINT Z AND LEAVE YEA FALSE.
function _Yes_, X, Y, Z

  compile_opt idl2, hidden

  common ALPHAS
  common VERSN
  common TXTCOM
  
;  if (X ne 0) then _RSPEAK_,X
  reply = ''
  while (TRUE) do begin
    if (DEBUG) then begin
      reply = (Randomu(s,1) le 0.5) ? 'YES' : 'NO'
    endif else begin
      if (WIDBASE ne 0) then begin
        reply = COMMAND
        COMMAND = ''
      endif else begin
        Read, reply, PROMPT='??? '
      endelse
    endelse
    reply = Strupcase(Strmid(reply,0,2))
    if (reply eq CYE || reply eq CY) then begin
      if (Y ne 0) then _RSPEAK_,Y
      Return, TRUE
    endif
    ; If using a widget, and they give a bad answer, just assume "no",
    ; to avoid having to come back here again via a widget event.
    if (reply eq CNO || reply eq CN || WIDBASE ne 0) then begin
      if (Z ne 0) then _RSPEAK_,Z
      Return, FALSE
    endif
    _Speak_, 'Please answer the question.'
  endwhile
end

;  STATEMENT FUNCTIONS


;  _Toting_(OBJ)  = TRUE IF THE OBJ IS BEING CARRIED
;  _Here_(OBJ)    = TRUE IF THE OBJ IS AT "LOC" (OR IS BEING CARRIED)
;  _At_(OBJ)      = TRUE IF ON EITHER SIDE OF TWO-PLACED OBJECT
;  _Liq_()   = OBJECT NUMBER OF LIQUID IN BOTTLE
;  _Liqloc_()  = OBJECT NUMBER OF LIQUID (IF ANY) AT LOC
;  _Bitset_(L,N)  = TRUE IF CONDITIONS[L] HAS BIT N SET (BIT 0 IS UNITS BIT)
;  _Forced_()  = TRUE IF LOC MOVES WITHOUT ASKING FOR INPUT (CONDITIONS = 2)
;  _Dark_()  = TRUE IF LOCATION "LOC" IS DARK
;  _Pct_(N)       = TRUE N% OF THE TIME (N INTEGER FROM 0 TO 100)

;  WZDARK SAYS WHETHER THE LOC HE'S LEAVING WAS DARK
;  LMWARN SAYS WHETHER HE'S BEEN WARNED ABOUT LAMP GOING DIM
;  CLOSNG SAYS WHETHER ITS CLOSING TIME YET
;  PANIC SAYS WHETHER HE'S FOUND OUT HE'S TRAPPED IN THE CAVE
;  CLOSED SAYS WHETHER WE'RE ALL THE WAY CLOSED
;  GAVEUP SAYS WHETHER HE EXITED VIA "QUIT"
;  SCORNG INDICATES TO THE SCORE ROUTINE WHETHER WE'RE DOING A "SCORE" COMMAND
;  DEMO IS TRUE IF THIS IS A PRIME-TIME DEMONSTRATION GAME
;  YEA IS RANDOM YES/NO REPLY

;--------------------------------------------------------------------
; These are logical "macros"
function _Toting_, object
  compile_opt idl2, hidden
  common PLACOM
  return, PLACE[object] eq -1
end

;--------------------------------------------------------------------
function _Here_, object
  compile_opt idl2, hidden
  common PLACOM
  common MISCOM
  return, PLACE[object] eq LOC || _Toting_(object)
end

;--------------------------------------------------------------------
function _At_, object
  compile_opt idl2, hidden
  common PLACOM
  common MISCOM
  return, PLACE[object] eq LOC || FIXED[object] eq LOC
end

;--------------------------------------------------------------------
function _Liq2_, PBOTL
  compile_opt idl2, hidden
  common MISCOM
  return, (1-PBOTL)*WATER+(PBOTL/2)*(WATER+OIL)
end

;--------------------------------------------------------------------
function _Liq_
  compile_opt idl2, hidden
  common MISCOM
  return, _Liq2_(MAX([PROP[BOTTLE],-1-PROP[BOTTLE]]))
end

;--------------------------------------------------------------------
function _Liqloc_
  compile_opt idl2, hidden
  common MISCOM
  return, _Liq2_((((CONDITIONS[LOC]/2*2) mod 8)-5)*((CONDITIONS[LOC]/4) mod 2)+1)
end

;--------------------------------------------------------------------
;     _Bitset_(L,N) = (CONDITIONS[L] && ISHFT(1,N)) ne 0
function _Bitset_,L,N
  compile_opt idl2, hidden
  common MISCOM
  return, (CONDITIONS[L] and ISHFT(1,N)) ne 0
end

;--------------------------------------------------------------------
function _Forced_, location
  compile_opt idl2, hidden
  common MISCOM
  return, CONDITIONS[location] eq 2
end

;--------------------------------------------------------------------
function _Dark_
  compile_opt idl2, hidden
  common VERSN
  common MISCOM
  if (debug) then return, FALSE
  return, (CONDITIONS[LOC] mod 2) eq 0 && (PROP[LAMP] eq 0 || ~_Here_(LAMP))
end

;--------------------------------------------------------------------
function _Rnd_, RANGE
  compile_opt idl2, hidden
  return, Fix(RANGE*Randomu(s,1))
end

;--------------------------------------------------------------------
function _Pct_, N
  compile_opt idl2, hidden
  return, (100*Randomu(s,1)) lt N
end

;--------------------------------------------------------------------
function _GetFile_, inGame
  compile_opt idl2, hidden
  if (Keyword_Set(inGame)) then begin
    dir = APP_USER_DIR('ITT', 'IDL', 'idladv', 'IDL Adventure', '', 1)
  endif else begin
    dir = (Routine_Info('IDL_Adventure_Init', /SOURCE)).path
    dir = File_dirname(dir, /MARK_DIRECTORY)
  endelse
  return, dir + 'idladv.sav'
end

;--------------------------------------------------------------------
pro _Savegm_, inGame
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
  file = _GetFile_(inGame)
  Save, FILE=file, /VARIABLES, /COMPRESS
  _SPEAK_, 'Your game has been saved.'
end

;--------------------------------------------------------------------
pro _Rstrgm_, inGame
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
  file = _GetFile_(inGame)
  if (~File_Test(file)) then begin
    _SPEAK_, 'There is no saved game.'
    return
  endif

  debugTmp = N_Elements(DEBUG) ? DEBUG : 0b
  wBaseTmp = N_Elements(WIDBASE) ? WIDBASE : 0L
  wTextTmp = N_Elements(WIDTEXT) ? WIDTEXT : 0L
  wInputTmp = N_Elements(WIDINPUT) ? WIDINPUT : 0L
  linesTmp = N_Elements(NLINES) ? NLINES : 0L
  
  Restore, FILE=file
  
  DEBUG = debugTmp
  WIDBASE = wBaseTmp
  WIDTEXT = wTextTmp
  WIDINPUT = wInputTmp
  NLINES = linesTmp
  
  if (Keyword_Set(inGame)) then $
    _SPEAK_, 'Your game has been restored.'
end


;--------------------------------------------------------------------
;  DESCRIPTION OF THE DATABASE FORMAT

;  THE DATA FILE CONTAINS SEVERAL SECTIONS.  EACH BEGINS WITH A LINE CONTAINING
;  A NUMBER IDENTIFYING THE SECTION, AND ENDS WITH A LINE CONTAINING "-1".

;  SECTION 1: LONG FORM DESCRIPTIONS.  EACH LINE CONTAINS A LOCATION NUMBER,
;       A COMMA, AND A LINE OF TEXT.  THE SET OF (NECESSARILY ADJACENT) LINES
;       WHOSE NUMBERS ARE X FORM THE LONG DESCRIPTION OF LOCATION X.
;  SECTION 2: SHORT FORM DESCRIPTIONS.  SAME FORMAT AS LONG FORM.  NOT ALL
;       PLACES HAVE SHORT DESCRIPTIONS.
;  SECTION 3: TRAVEL TABLE.  EACH LINE CONTAINS A LOCATION NUMBER [X], A SECOND
;       LOCATION NUMBER [Y], AND A LIST OF MOTION NUMBERS (SEE SECTION 4).
;       EACH MOTION REPRESENTS A VERB WHICH WILL GO TO Y IF CURRENTLY AT X.
;       Y, IN TURN, IS INTERPRETED AS FOLLOWS.  LET M = Y/1000, N = Y MOD 1000.
;               IF N< = 300       IT IS THE LOCATION TO GO TO.
;               IF 300<N< = 500   N-300 IS USED IN A COMPUTED goto, LTO
;                                       A SECTION OF SPECIAL CODE.
;               IF N>500        MESSAGE N-500 FROM SECTION 6 IS PRINTED,
;                                       AND HE STAYS WHEREVER HE IS.
;       MEANWHILE, M SPECIFIES THE CONDITIONS ON THE MOTION.
;               IF M = 0          IT'S UNCONDITIONAL.
;               IF 0<M<100      IT IS DONE WITH M% PROBABILITY.
;               IF M = 100        UNCONDITIONAL, BUT FORBIDDEN TO DWARVES.
;               IF 100<M< = 200   HE MUST BE CARRYING OBJECT M-100.
;               IF 200<M< = 300   MUST BE CARRYING OR IN SAME ROOM AS M-200.
;               IF 300<M< = 400   PROP(M MOD 100) MUST *NOT* BE 0.
;               IF 400<M< = 500   PROP(M MOD 100) MUST *NOT* BE 1.
;               IF 500<M< = 600   PROP(M MOD 100) MUST *NOT* BE 2, ETC.
;       IF THE CONDITION (IF ANY) IS NOT MET, THEN THE NEXT *DIFFERENT*
;       "DESTINATION" VALUE IS USED (UNLESS IT FAILS TO MEET *ITS* CONDITIONS,
;       IN WHICH CASE THE NEXT IS FOUND, ETC.).  TYPICALLY, THE NEXT DEST WILL
;       BE FOR ONE OF THE SAME VERBS, SO THAT ITS ONLY USE IS AS THE ALTERNATE
;       DESTINATION FOR THOSE VERBS.  FOR INSTANCE:
;               15      110022  29      31      34      35      23      43
;               15      14      29
;       THIS SAYS THAT, FROM LOC 15, ANY OF THE VERBS 29, 31, ETC., WILL TAKE
;       HIM TO 22 IF HE'S CARRYING OBJECT 10, AND OTHERWISE WILL GO TO 14.
;               11      303008  49
;               11      9       50
;       THIS SAYS THAT, FROM 11, 49 TAKES HIM TO 8 UNLESS PROP[3] = 0, IN WHICH
;       CASE HE GOES TO 9.  VERB 50 TAKES HIM TO 9 REGARDLESS OF PROP[3].

;       IN THIS IMPLEMENTATION, THE SECOND LOCATION NUMBER Y HAS BEEN
;       SPLIT INTO M, CONDITIONS, AND N, LOCATION.

;  SECTION 4: VOCABULARY.  EACH LINE CONTAINS A NUMBER [N], A TAB, AND A
;       FIVE-LETTER WORD.  CALL M = N/1000.  IF M = 0, THEN THE WORD IS A MOTION
;       VERB FOR USE IN TRAVELLING (SEE SECTION 3).  ELSE, IF M = 1, THE WORD IS
;       AN OBJECT.  ELSE, IF M = 2, THE WORD IS AN ACTION VERB (SUCH AS "CARRY"
;       OR "ATTACK").  ELSE, IF M = 3, THE WORD IS A SPECIAL CASE VERB (SUCH AS
;       "DIG") AND N MOD 1000 IS AN INDEX INTO SECTION 6.  OBJECTS FROM 50 TO
;       (CURRENTLY, ANYWAY) 79 ARE CONSIDERED TREASURES (FOR PIRATE, CLOSEOUT).
;  SECTION 5: OBJECT DESCRIPTIONS.  EACH LINE CONTAINS A NUMBER [N], A TAB,
;       AND A MESSAGE.  IF N IS FROM 1 TO 100, THE MESSAGE IS THE "INVENTORY"
;       MESSAGE FOR OBJECT N.  OTHERWISE, N SHOULD BE 000, 100, 200, ETC., AND
;       THE MESSAGE SHOULD BE THE DESCRIPTION OF THE PRECEDING OBJECT WHEN ITS
;       PROP VALUE IS N/100.  THE N/100 IS USED ONLY TO DISTINGUISH MULTIPLE
;       MESSAGES FROM MULTI-LINE MESSAGES; THE PROP INFO ACTUALLY REQUIRES ALL
;       MESSAGES FOR AN OBJECT TO BE PRESENT AND CONSECUTIVE.  PROPERTIES WHICH
;       PRODUCE NO MESSAGE SHOULD BE GIVEN THE MESSAGE ">$<".
;  SECTION 6: ARBITRARY MESSAGES.  SAME FORMAT AS SECTIONS 1, 2, AND 5, EXCEPT
;       THE NUMBERS BEAR NO RELATION TO ANYTHING (EXCEPT FOR SPECIAL VERBS
;       IN SECTION 4).
;  SECTION 7: OBJECT LOCATIONS.  EACH LINE CONTAINS AN OBJECT NUMBER AND ITS
;       INITIAL LOCATION (ZERO (OR OMITTED) IF NONE).  IF THE OBJECT IS
;       IMMOVABLE, THE LOCATION IS FOLLOWED BY A "-1".  IF IT HAS TWO LOCATIONS
;       (E.G. THE GRATE) THE FIRST LOCATION IS FOLLOWED WITH THE SECOND, AND
;       THE OBJECT IS ASSUMED TO BE IMMOVABLE.
;  SECTION 8: ACTION DEFAULTS.  EACH LINE CONTAINS AN "ACTION-VERB" NUMBER AND
;       THE INDEX (IN SECTION 6) OF THE DEFAULT MESSAGE FOR THE VERB.
;  SECTION 9: LIQUID ASSETS, ETC.  EACH LINE CONTAINS A NUMBER [N] AND UP TO 20
;       LOCATION NUMBERS.  BIT N (WHERE 0 IS THE UNITS BIT) IS SET IN CONDITIONS[LOC]
;       FOR EACH LOC GIVEN.  THE CONDITIONS BITS CURRENTLY ASSIGNED ARE:
;               0       LIGHT
;               1       IF BIT 2 IS ON: ON FOR OIL, OFF FOR WATER
;               2       LIQUID ASSET, SEE BIT 1
;               3       PIRATE DOESN'T GO HERE UNLESS FOLLOWING PLAYER
;       OTHER BITS ARE USED TO INDICATE AREAS OF INTEREST TO "HINT" ROUTINES:
;               4       TRYING TO GET INTO CAVE
;               5       TRYING TO CATCH BIRD
;               6       TRYING TO DEAL WITH SNAKE
;               7       LOST IN MAZE
;               8       PONDERING DARK ROOM
;               9       AT WITT'S END
;       CONDITIONS[LOC] IS SET TO 2, OVERRIDING ALL OTHER BITS, IF LOC HAS FORCED
;       MOTION.
;  SECTION 10: CLASS MESSAGES.  EACH LINE CONTAINS A NUMBER [N], A TAB, AND A
;       MESSAGE DESCRIBING A CLASSIFICATION OF PLAYER.  THE SCORING SECTION
;       SELECTS THE APPROPRIATE MESSAGE, WHERE EACH MESSAGE IS CONSIDERED TO
;       APPLY TO PLAYERS WHOSE SCORES ARE HIGHER THAN THE PREVIOUS N BUT NOT
;       HIGHER THAN THIS N.  NOTE THAT THESE SCORES PROBABLY CHANGE WITH EVERY
;       MODIFICATION (AND PARTICULARLY EXPANSION) OF THE PROGRAM.
;  SECTION 11: HINTS.  EACH LINE CONTAINS A HINT NUMBER (CORRESPONDING TO A
;       CONDITIONS BIT, SEE SECTION 9), THE NUMBER OF TURNS HE MUST BE AT THE RIGHT
;       LOC[S] BEFORE TRIGGERING THE HINT, THE POINTS DEDUCTED FOR TAKING THE
;       HINT, THE MESSAGE NUMBER (SECTION 6) OF THE QUESTION, AND THE MESSAGE
;       NUMBER OF THE HINT.  THESE VALUES ARE STASHED IN THE "HINTS" ARRAY.
;       HNTMAX IS SET TO THE MAX HINT NUMBER (< =  HNTSIZ).  NUMBERS 1-3 ARE
;       UNUSABLE SINCE CONDITIONS BITS ARE OTHERWISE ASSIGNED, SO 2 IS USED TO
;       REMEMBER IF HE'S READ THE CLUE IN THE REPOSITORY, AND 3 IS USED TO
;       REMEMBER WHETHER HE ASKED FOR INSTRUCTIONS (GETS MORE TURNS, BUT LOSES
;       POINTS).
;  SECTION 0: END OF DATABASE.
;
pro IDL_Adventure_Init

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

  TRUE = 1b
  FALSE = 0b
  if (N_Elements(DEBUG) eq 0) then DEBUG = 0
  
; Try to restore our cached initial state. If success, return early.
; CT, May 2008: Don't bother to cache state. It's fast enough.
;  _RSTRGM_, FALSE
;  if (N_Elements(BLANK) gt 0) then return

  BLANK = ' '
  CYE = 'YE'
  CNO = 'NO'
  EOF = '>$'
  CY = 'Y'
  CN = 'N'
  CPO = 'PO'
  CUR = 'UR'
  CWE = 'WE'
  CST = 'ST'
  CEN = 'EN'
  CTE = 'TE'
  
  VMAJ = 3
  VMIN = 0
  VEDIT = 'A'
  
  FILSIZ = 900
  TABSIZ = 300
  LOCSIZ = 150
  VRBSIZ = 35
  RTXSIZ = 205
  HNTSIZ = 20
  TRVSIZ = 750
  CLSMAX = 12
  WZDARK = FALSE
  
  
  ; Motion
  TRAVEL = Intarr(TRVSIZ+1)
  TRVCON = Intarr(TRVSIZ+1)
  TRVLOC = Intarr(TRVSIZ+1)
  
  ; Vocabulary
  KTAB = Intarr(TABSIZ+1)
  ATAB = Strarr(TABSIZ+1)
  A2TAB = Strarr(TABSIZ+1)
  
  ; Descriptions
  LTEXT = Strarr(LOCSIZ+1)
  STEXT = Strarr(LOCSIZ+1)
  
  ; Locations
  KEY = Intarr(LOCSIZ+1)
  CONDITIONS = Intarr(LOCSIZ+1)
  ABB = Intarr(LOCSIZ+1)
  ATLOC = Intarr(LOCSIZ+1)
  
  ; Objects
  PLAC = Intarr(101)
  PLACE = Intarr(101)
  FIXD = Intarr(101)
  FIXED = Intarr(101)
  LINK = Intarr(201)
  PTEXT = Strarr(101,7)
  PROP = Intarr(101)
  
  ; Action verbs
  ACTSPK = Intarr(35)
  
  ; Messages
  RTEXT = Strarr(205)
  
  ; Player classification
  CTEXT = Strarr(12)
  CVAL = Intarr(12)
  
  ; Hints
;  CLEAR THE HINT STUFF.  HINTLC[I] IS HOW LONG HE'S BEEN AT LOC WITH CONDITIONS BIT
;  I.  HINTED[I] IS TRUE IFF HINT I HAS BEEN USED.
  HINTLC = Intarr(HNTSIZ+1)
  HINTED = Bytarr(HNTSIZ+1)
  HINTS = Intarr(HNTSIZ+1,5)
  
  TK = Intarr(20)
  
  ; Dwarfs
  DSEEN = Bytarr(7)
  DLOC = Intarr(7)
  ODLOC = Intarr(7)
  
  path = FILE_DIRNAME(Routine_Filepath('IDL_Adventure_Init'), /MARK)
  Openr,lun, path + 'atext.txt', /GET_LUN
  section = ''
  
  ; Section 1: Long descriptions
  Readf, lun, section
  location = 0
  description = ''
  while (TRUE) do begin
    Readf, lun, location, description, FORMAT='(I, A)'
    if (location eq -1) then break
    if (LTEXT[location] ne '') then LTEXT[location] += String(10b)
    LTEXT[location] += description
  endwhile
  if (DEBUG) then Print, section, ' ', LTEXT[140]

  ; Section 2: Short descriptions
  Readf, lun, section
  while (TRUE) do begin
    Readf, lun, location, description, FORMAT='(I, A)'
    if (location eq -1) then break
    if (STEXT[location] ne '') then STEXT[location] += String(10b)
    STEXT[location] += description
  endwhile
  if (DEBUG) then Print, section, ' ', STEXT[130]

; Section 3: Travel table
;  THE STUFF FOR SECTION 3 IS ENCODED HERE.  EACH "FROM-LOCATION" GETS A
;  CONTIGUOUS SECTION OF THE "TRAVEL" ARRAY.  EACH ENTRY IN TRAVEL IS
;  KEYWORD (FROM SECTION 4, MOTION VERBS), AND IS NEGATED IF
;  THIS IS THE LAST ENTRY FOR THIS LOCATION.  KEY[N] IS THE INDEX IN TRAVEL
;  OF THE FIRST OPTION AT LOCATION N.

;  SPECIAL CONDITIONS ON TRAVEL ARE ENCODED IN THE CORRESPONDING
;  ENTRIES OF TRVCON.  THE NEW LOCATION IS IN TRVLOC.
  Readf, lun, section
  loc = 0
  condy = 0
  newloc = 0
  trvs = 1
  s = ''
  while (TRUE) do begin
    Readf, lun, loc, s, FORMAT='(I, A)'
    if (loc eq -1) then break
    Reads, s, condy, newloc, description, FORMAT='(2I, A)'
    if (KEY[loc] ne 0) then begin
      ; More travel entries, make the previous positive again.
      TRAVEL[trvs-1] = Abs(TRAVEL[trvs-1])
    endif else begin
      KEY[loc] = trvs
    endelse
    motion = Fix(Strtok(description,',',/EXTRACT))
    for L = 0, N_Elements(motion)-1 do begin
      if (motion[L] eq 0) then break
      TRAVEL[trvs] = motion[L]
      TRVLOC[trvs] = newloc
      TRVCON[trvs] = condy
      trvs++
      if (trvs eq TRVSIZ) then Message, 'TOO MANY TRAVEL OPTIONS'
    endfor
    ; Assume this is the end of the travel entries.
    TRAVEL[trvs-1] = -TRAVEL[trvs-1]
  endwhile
  if (DEBUG) then begin
    Print, section, KEY[140], $
      TRAVEL[KEY[140]], TRVLOC[KEY[140]], TRVCON[KEY[140]]
  endif
  
; Section 4: Vocabulary
;  HERE WE READ IN THE VOCABULARY.  KTAB[N] IS THE WORD NUMBER, ATAB[N] IS
;  THE CORRESPONDING WORD.  THE -1 AT THE END OF SECTION 4 IS LEFT IN KTAB
;  AS AN END-MARKER.
  Readf, lun, section
  TABNDX = 1
  number = 0
  word = ''
  ; THE -1 AT THE END OF SECTION 4 IS LEFT IN KTAB
  ;  AS AN END-MARKER.
  while (TRUE) do begin
    Readf, lun, number, word, FORMAT='(I, A)'
    KTAB[TABNDX] = number
    ATAB[TABNDX] = Strmid(word,0,2)
    A2TAB[TABNDX] = Strmid(word,2,2)
    if (number eq -1) then break
    TABNDX++
  endwhile
  if (TABNDX gt TABSIZ) then Message, 'TOO MANY VOCABULARY WORDS'
  TABNDX--
  if (DEBUG) then Print, section, TABNDX, KTAB[TABNDX], ' ', ATAB[TABNDX], A2TAB[TABNDX]

; Section 5: Object descriptions
;  READ IN THE INITIAL LOCATIONS FOR EACH OBJECT.  ALSO THE IMMOVABILITY INFO.
;  PLAC CONTAINS INITIAL LOCATIONS OF OBJECTS.  FIXD IS -1 FOR IMMOVABLE
;  OBJECTS (INCLUDING THE SNAKE), OR = SECOND LOC FOR TWO-PLACED OBJECTS.
  Readf, lun, section
  while (TRUE) do begin
    Readf, lun, description
    if (Strmid(description,0,2) eq '-1') then break
    comma = Strpos(description, ',')
    text = Strmid(description, comma+1)
    if (comma eq 3) then begin  ; three digit obj description
      propId = 1 + Fix(Strmid(description,0,comma))/100
    endif else begin  ; starting a new object
      number = Fix(Strmid(description,0,comma))
      propId = 0
    endelse
    if (PTEXT[number,propId] ne '') then PTEXT[number,propId] += String(10b)
    PTEXT[number,propId] += text
  endwhile
  if (DEBUG) then Print, section, PTEXT[24,3]
  
  ; Section 6: Arbitrary messages
  Readf, lun, section
  while (TRUE) do begin
    Readf, lun, location, description, FORMAT='(I, A)'
    if (location eq -1) then break
    if (RTEXT[location] ne '') then RTEXT[location] += String(10b)
    RTEXT[location] += description
  endwhile
  if (DEBUG) then Print, section, ' ', RTEXT[201]
  
  ; Section 7: Object locations
  Readf, lun, section
  while (TRUE) do begin
    Readf, lun, number, description, FORMAT='(I, A)'
    if (number eq -1) then break
    loc = Fix(Strtok(description,',',/EXTRACT))
    PLAC[number] = loc[0]
    if (N_Elements(loc) eq 2) then $
      FIXD[number] = loc[1]
  endwhile
  if (DEBUG) then Print, section, PLAC[64], FIXD[64]
  
  ; Section 8: Action defaults
  Readf, lun, section
  index = 0
  while (TRUE) do begin
    Readf, lun, number, description, FORMAT='(I, A)'
    if (number eq -1) then break
    ACTSPK[number] = Fix(description)
  endwhile
  if (DEBUG) then Print, section, ACTSPK[32]

  ; Section 9: Liquid assets, etc.
  Readf, lun, section
  while (TRUE) do begin
    Readf, lun, number, description, FORMAT='(I, A)'
    if (number eq -1) then break
    loc = Fix(Strtok(description,',',/EXTRACT))
    bitToSet = ISHFT(1,number)
    for i = 0,N_Elements(loc)-1 do begin
      if (loc[i] eq 0) then break
      bitset = (CONDITIONS[loc[i]] and bitToSet) ne 0
      if (bitset) then Message, 'LOCATION HAS CONDITIONS BIT BEING SET TWICE'
      CONDITIONS[loc[i]] += bitToSet
    endfor
  endwhile
  if (DEBUG) then Print, section, CONDITIONS[108]

  ; Section 10: Class messages
  Readf, lun, section
  clsses = 0  ; this will actually start at 1 (see below)
  while (TRUE) do begin
    Readf, lun, number, description, FORMAT='(I, A)'
    if (number eq -1) then break
    clsses++
    CTEXT[clsses] = description
    CVAL[clsses] = number
  endwhile
  if (DEBUG) then Print, section, CVAL[clsses], ' ', CTEXT[clsses]
  
  ; Section 11: Hints
  Readf, lun, section
  while (TRUE) do begin
    Readf, lun, number, description, FORMAT='(I, A)'
    if (number eq -1) then break
    HINTS[number, 1:*] = Fix(Strtok(description,',',/EXTRACT))
    HNTMAX = number
  endwhile
  if (DEBUG) then Print, section, Reform(HINTS[9,*])
  
  ; Section 0: End of database
  Readf, lun, section
  if (DEBUG) then Print, section
  
  Free_lun, lun
  
;  HAVING READ IN THE DATABASE, CERTAIN THINGS ARE NOW CONSTRUCTED.  PROPS ARE
;  SET TO ZERO.  WE FINISH SETTING UP CONDITIONS BY CHECKING FOR FORCED-MOTION TRAVEL
;  ENTRIES.  THE PLAC AND FIXD ARRAYS ARE USED TO SET UP ATLOC[N] AS THE FIRST
;  OBJECT AT LOCATION N, AND LINK[OBJ] AS THE NEXT OBJECT AT THE SAME LOCATION
;  AS OBJ.  (OBJ>100 INDICATES THAT FIXED(OBJ-100) = LOC; LINK[OBJ] IS STILL THE
;  CORRECT LINK TO USE.)  ABB IS ZEROED; IT CONTROLS WHETHER THE ABBREVIATED
;  DESCRIPTION IS PRINTED.  COUNTS MOD 5 UNLESS "LOOK" IS USED.


;  IF THE FIRST MOTION VERB IS 1 [ILLEGAL], THEN THIS IS A FORCED
;  MOTION ENTRY.

  for i = 1,LOCSIZ do begin
      if (LTEXT[i] eq '' || KEY[i] eq 0) then continue
      k = KEY[i]
      if (ABS(TRAVEL[k]) eq 1) then CONDITIONS[i] = 2
  endfor

;  SET UP THE ATLOC AND LINK ARRAYS AS DESCRIBED ABOVE.  WE'LL USE THE DROP
;  SUBROUTINE, WHICH PREFACES NEW OBJECTS ON THE LISTS.  SINCE WE WANT THINGS
;  IN THE OTHER ORDER, WE'LL RUN THE LOOP BACKWARDS.  IF THE OBJECT IS IN TWO
;  LOCS, WE DROP IT TWICE.  THIS ALSO SETS UP "PLACE" AND "FIXED" AS COPIES OF
;  "PLAC" AND "FIXD".  ALSO, SINCE TWO-PLACED OBJECTS ARE TYPICALLY BEST
;  DESCRIBED LAST, WE'LL DROP THEM FIRST.

  for i = 1,100 do begin
      k = 101 - i
      if (FIXD[k] le 0) then continue
      kp100 = k + 100
      _Drop_, kp100, FIXD[k]
      _Drop_, k, PLAC[k]
  endfor

  for i = 1,100 do begin
    K = 101 - I
    FIXED[K] = FIXD[K]
    if (PLAC[K] ne 0 && FIXD[K] le 0) then _Drop_, K,PLAC[K]
  endfor
  
;  TREASURES, AS NOTED EARLIER, ARE OBJECTS 50 THROUGH MAXTRS (CURRENTLY 79).
;  THEIR PROPS ARE INITIALLY -1, AND ARE SET TO 0 THE FIRST TIME THEY ARE
;  DESCRIBED.  TALLY KEEPS TRACK OF HOW MANY ARE NOT YET FOUND, SO WE KNOW
;  WHEN TO CLOSE THE CAVE.  TALLY2 COUNTS HOW MANY CAN NEVER BE FOUND (E.G. IF
;  LOST BIRD OR BRIDGE).

  MAXTRS= 79
  TALLY = 0
  TALLY2 = 0
  for I = 50,MAXTRS do begin
    if (PTEXT[I,0] ne '') then PROP[I] = -1
    TALLY= TALLY-PROP[I]
  endfor
  
  ;  DEFINE SOME HANDY MNEMONICS.  THESE CORRESPOND TO OBJECT NUMBERS.
  
  _Vocab_, 'KE','YS',1,KEYS
  _Vocab_, 'LA','MP',1,LAMP
  _Vocab_, 'GR','AT',1,GRATE
  _Vocab_, 'CA','GE',1,CAGE
  _Vocab_, 'RO','D ',1,ROD
  ROD2 = ROD+1
  _Vocab_, 'ST','EP',1,STEPS
  _Vocab_, 'BI','RD',1,BIRD
  _Vocab_, 'DO','OR',1,DOOR
  _Vocab_, 'PI','LL',1,PILLOW
  _Vocab_, 'SN','AK',1,SNAKE
  _Vocab_, 'FI','SS',1,FISSURE
  _Vocab_, 'TA','BL',1,TABLET
  _Vocab_, 'CL','AM',1,CLAM
  _Vocab_, 'OY','ST',1,OYSTER
  _Vocab_, 'MA','GA',1,MAGZIN
  _Vocab_, 'DW','AR',1,DWARF
  _Vocab_, 'KN','IF',1,KNIFE
  _Vocab_, 'FO','OD',1,FOOD
  _Vocab_, 'BO','TT',1,BOTTLE
  _Vocab_, 'WA','TE',1,WATER
  _Vocab_, 'OI','L', 1,OIL
  _Vocab_, 'PL','AN',1,PLANT
  PLANT2 = PLANT+1
  _Vocab_, 'AX','E', 1,AXE
  _Vocab_, 'MI','RR',1,MIRROR
  _Vocab_, 'DR','AG',1,DRAGON
  _Vocab_, 'CH','AS',1,CHASM
  _Vocab_, 'TR','OL',1,TROLL
  TROLL2 = TROLL+1
  _Vocab_, 'BE','AR',1,BEAR
  _Vocab_, 'ME','SS',1,MESSAG
  _Vocab_, 'VE','ND',1,VEND
  _Vocab_, 'BA','TT',1,BATTER
  
  ;  OBJECTS FROM 50 THROUGH WHATEVER ARE TREASURES.  HERE ARE A FEW.
  
  _Vocab_, 'GO','LD',1,NUGGET
  _Vocab_, 'CO','IN',1,COINS
  _Vocab_, 'CH','ES',1,CHEST
  _Vocab_, 'EG','GS',1,EGGS
  _Vocab_, 'TR','ID',1,TRIDNT
  _Vocab_, 'VA','SE',1,VASE
  _Vocab_, 'EM','ER',1,EMRALD
  _Vocab_, 'PY','RA',1,PYRAM
  _Vocab_, 'PE','AR',1,PEARL
  _Vocab_, 'RU','G', 1,RUG
  _Vocab_, 'CH','AI',1,CHAIN
  _Vocab_, 'SP','IC',1,SPICES
  
  ;  THESE ARE MOTION-VERB NUMBERS.
  
  _Vocab_, 'BA','CK',0,BACK
  _Vocab_, 'LO','OK',0,LOOK
  _Vocab_, 'CA','VE',0,CAVE
  _Vocab_, 'NU','LL',0,NULL
  _Vocab_, 'EN','TR',0,ENTRNC
  _Vocab_, 'DE','PR',0,DPRSSN
  _Vocab_, 'ST','RE',0,STREAM
  
  ;  AND SOME ACTION VERBS.
  
  _Vocab_, 'SA','Y', 2,SAY
  _Vocab_, 'LO','CK',2,LOCK
  _Vocab_, 'TH','RO',2,THROW
  _Vocab_, 'FI','ND',2,FIND
  _Vocab_, 'IN','VE',2,INVENT


;  INITIALISE THE DWARVES.  DLOC IS LOC OF DWARVES, HARD-WIRED IN.  ODLOC IS
;  PRIOR LOC OF EACH DWARF, INITIALLY GARBAGE.  DALTLC IS ALTERNATE INITIAL LOC
;  FOR DWARF, IN CASE ONE OF THEM STARTS OUT ON TOP OF THE ADVENTURER.  (NO 2
;  OF THE 5 INITIAL LOCS ARE ADJACENT.)  DSEEN IS TRUE IF DWARF HAS SEEN HIM.
;  DFLAG CONTROLS THE LEVEL OF ACTIVATION OF ALL THIS:
;       0       NO DWARF STUFF YET (WAIT UNTIL REACHES HALL OF MISTS)
;       1       REACHED HALL OF MISTS, BUT HASN'T MET FIRST DWARF
;       2       MET FIRST DWARF, OTHERS START MOVING, NO KNIVES THROWN YET
;       3       A KNIFE HAS BEEN THROWN (FIRST SET ALWAYS MISSES)
;       3+      DWARVES ARE MAD (INCREASES THEIR ACCURACY)
;  SIXTH DWARF IS SPECIAL (THE PIRATE).  HE ALWAYS STARTS AT HIS CHEST'S
;  EVENTUAL LOCATION INSIDE THE MAZE.  THIS LOC IS SAVED IN CHLOC FOR REF.
;  THE DEAD END IN THE OTHER MAZE HAS ITS LOC STORED IN CHLOC2.

      CHLOC = 114
      CHLOC2 = 140
      DFLAG = 0
      DLOC[1] = 19
      DLOC[2] = 27
      DLOC[3] = 33
      DLOC[4] = 44
      DLOC[5] = 64
      DLOC[6] = CHLOC
      DALTLC = 18

;  OTHER RANDOM FLAGS AND COUNTERS, AS FOLLOWS:
;       TURNS   TALLIES HOW MANY COMMANDS HE'S GIVEN (IGNORES YES/NO)
;       LIMIT   LIFETIME OF LAMP (NOT SET HERE)
;       KNFLOC  0 IF NO KNIFE HERE, LOC IF KNIFE HERE, -1 AFTER CAVEAT
;       DETAIL  HOW OFTEN WE'VE SAID "NOT ALLOWED TO GIVE MORE DETAIL"
;       ABBNUM  HOW OFTEN WE SHOULD PRINT NON-ABBREVIATED DESCRIPTIONS
;       MAXDIE  NUMBER OF REINCARNATION MESSAGES AVAILABLE (UP TO 5)
;       NUMDIE  NUMBER OF TIMES KILLED SO FAR
;       HOLDNG  NUMBER OF OBJECTS BEING CARRIED
;       DKILL   NUMBER OF DWARVES KILLED (UNUSED IN SCORING, NEEDED FOR MSG)
;       FOOBAR  CURRENT PROGRESS IN SAYING "FEE FIE FOE FOO".
;       BONUS   USED TO DETERMINE AMOUNT OF BONUS IF HE REACHES CLOSING
;       CLOCK1  NUMBER OF TURNS FROM FINDING LAST TREASURE TILL CLOSING
;       CLOCK2  NUMBER of TURNS FROM FIRST WARNING TILL BLINDING FLASH
;       LOGICALS WERE EXPLAINED EARLIER

  TURNS = 0
  LMWARN = FALSE
  KNFLOC = 0
  DETAIL = 0
  ABBNUM = 5
  for i = 0,4 do begin
    if (RTEXT[2*I+81] ne '') then MAXDIE = I+1
  endfor
  NUMDIE = 0
  HOLDNG = 0
  DKILL = 0
  FOOBAR = 0
  BONUS = 0
  CLOCK1 = 30
  CLOCK2 = 50
  CLOSNG = FALSE
  PANIC = FALSE
  CLOSED = FALSE
  GAVEUP = FALSE
  SCORNG = FALSE

; Save our cached initial state, for faster startup next time.
; CT, May 2008: Don't bother to cache state. It's fast enough.
;  _SAVEGM_, FALSE

end

