.MODEL COMPACT
.STACK 64

;Macroos
DisplayString MACRO STR
    PushALL
    mov Ah,09h
    mov dx, offset STR
    int 21h    
    PopALL
ENDM DisplayString

DisplayStringRead MACRO STR
    mov Ah,09h
    mov dx, offset STR+2
    int 21h    
ENDM DisplayString

ReadString MACRO PromptMessege
    mov Ah,0Ah 
    mov Dx, Offset PromptMessege
    int 21h        
ENDM ReadString     

MoveCursor MACRO X,Y
    mov Ah,02H
    Mov Dl,X
    Mov Dh,Y
    int 10h    
ENDM MoveCursor

;New Fixed line with constant start X and variable Y
NewFLine MACRO X,Step
    PushALL
    ;Geting cursor position   
    mov Ah,03h
    mov bh,0h
    int 10h
    ;Dl=X-Position Dh=Y-Position

    Add DH,Step ;To reach new line

    ;Set Cursor at new line
    mov Ah,02H
    Mov Dl,X
    int 10h  
    PopAll  
ENDM NewFLine  

PushALL MACRO
    Push AX
    Push BX
    Push CX
    Push DX
ENDM PushALL

PopALL MACRO
    Pop DX
    Pop CX
    Pop BX
    Pop Ax
ENDM PopALL

Clear MACRO 
    mov Ax,3
    int 10h
ENDM Clear  

StringToNumber Macro  InputNumber 
    Local MyLoop,Num,Continue
    Mov SI,offset InputNumber
    Mov Cl,[SI+1]
    MyLoop:
    Mov Al,[SI+2]
    CMP Al,40H   ;If less (Number)   // More (Letter)
    JC Num
    Sub Al,37H
    JMP Continue
    Num: Sub Al,30H
    Continue:
    Mov [SI+2],Al
    INC SI
    DEC Cl
    JNZ MyLoop
ENDM StringToNumber

NumberToString Macro InputNumber,DispNumber
    Local LOOPX,Num,ContinueFirst,ContinueSecond,Num1
    PushALL
        Mov SI,offset InputNumber
        Mov DI,offset DispNumber+3
        Mov CL,2
        LOOPX:
        Mov Al,[SI]
        Mov DL,00001111B
        AND Al,Dl   ;To get the first 4 bits
        CMP Al,1010B
        JB Num
        Add Al,37H
        JMP ContinueFirst
        Num: Add AL,30H
        ContinueFirst:
        Mov [DI],Al  ;To put the first byte
        DEC DI
        Mov Al,[SI]
        Push CX
        Mov Cl,4
        SHR Al,Cl
        Pop Cx
        CMP Al,1010B
        JB Num1
        Add Al,37H
        JMP ContinueSecond
        Num1: Add AL,30H
        ContinueSecond:
        Mov [DI],Al
        DEC DI
        INC SI
        DEC CL
        CMP Cl,0
        JNZ LOOPX
    PopAll
ENDM NumberToString

MemoryToString Macro DispNumber
    Local LOOPX,Num,ContinueFirst,ContinueSecond,Num1
    PushALL
    PUSH SI 
    PUSH DI
        Mov DI,offset DispNumber+1
        Mov Al,[SI]
        Mov DL,00001111B
        AND Al,Dl   ;To get the first 4 bits
        CMP Al,1010B
        JB Num
        Add Al,37H
        JMP ContinueFirst
        Num: Add AL,30H
        ContinueFirst:
        Mov [DI],Al  ;To put the first byte
        DEC DI
        Mov Al,[SI]
        Push CX
        Mov Cl,4
        SHR Al,Cl
        Pop Cx 
        CMP Al,1010B
        JB Num1
        Add Al,37H
        JMP ContinueSecond
        Num1: Add AL,30H
        ContinueSecond:
        Mov [DI],Al
    POP DI 
    POP SI              
    POPALL
ENDM MemoryToString

STRINGCOPY Macro M1,M2,Size
    PushALL
    PUSH DI
    mov AX,DS 
    mov ES,AX
    mov SI,offset M1 
    mov DI,offset M2
    Mov Cx,0
    mov cl,Size
    REP MOVSB
    POP DI
    PopALL
ENDM STRINGCOPY

STRINGCOMPARE Macro M1,M2,Size,Indicator
    Local LOP,X
    PushALL
    mov AX,DS 
    mov ES,AX
    mov SI,offset M1 
    mov DI,offset M2 + 2
    Mov Cx,Size
    REPE CMPSB

    CMP CX,0
    JZ LOP
    Mov Indicator,0
    JMP X
    LOP: Mov Indicator,1
    X:
    PopALL
ENDM STRINGCOMPARE

DrawPixel Macro X,Y,Color
    mov bh,0
    mov al, Color
    mov cx, X
    mov dx, Y
    mov ah, 0ch
    int 10h 
ENDM DrawPixel

DisplayCharacter Macro Letter,Color
    ;bh=Page Number, al=Letter ASCII ,CX=Number of times,bl=Colour
    mov ah,9 
    mov bh,0
    mov al,letter
    mov cx,1
    mov bl,Color
    int 10h
ENDM DisplayCharacter

DisplayCharacterSTR Macro Color,X,Y,Size
    Local STRLOOP 
    PushALL
    Mov DI,Size
    Mov DL,X   
    STRLOOP:   
        mov Ah,02H
        Mov Dh,Y
        int 10h  
        
        
        ;bh=Page Number, al=Letter ASCII ,CX=Number of times,bl=Colour
        mov ah,9 
        mov bh,0
        mov al,[SI]
        mov cx,1
        mov bl,Color
        int 10h

        
        INC SI
        INC DL

    DEC DI
    JNZ STRLOOP
    PopALL
ENDM DisplayCharacterSTR

ReadCharacter Macro x
    PushALL
    mov ah, 1
    int 21h 
    Mov x,al
    PopALL
ENDM ReadCharacter

DrawVLinePixel Macro X,Y1,Y2,Color
    Local LineLoop
    PushALL
    Mov SI,Y1
    LineLoop:
    DrawPixel X,SI,Color
    INC SI
    CMP SI,Y2
    JNZ LineLoop
    PopALL
ENDM DrawVLinePixel

DrawHLinePixel Macro X1,X2,Y,Color
    Local LineLoop1
    PushALL
    Mov SI,X1
    LineLoop1:
    DrawPixel SI,Y,Color
    INC SI
    CMP SI,X2
    JNZ LineLoop1
    PopALL
ENDM DrawHLinePixel

DrawDiagonalRLinePixel Macro X1,X2,Y1,Color
    Local LineLoop1
    PushALL
    Mov SI,X1
    Mov DI,Y1
    LineLoop1:
    DrawPixel SI,DI,Color
    INC SI
    INC DI
    CMP SI,X2
    JNZ LineLoop1
    PopALL
ENDM DrawDiagonalRLinePixel

DrawDiagonalPLinePixel Macro X1,X2,Y1,Color
    Local LineLoop1
    PushALL
    Mov SI,X1
    Mov DI,Y1
    LineLoop1:
    DrawPixel SI,DI,Color
    INC SI
    Dec DI
    CMP SI,X2
    JNZ LineLoop1
    PopALL
ENDM DrawDiagonalPLinePixel

DrawSquarePixel Macro X1,X2,Y1,Y2,Color
    Local SquareLoop
    PushALL
    Mov DI,Y1
    SquareLoop:
    DrawHLinePixel X1,X2,DI,Color
    INC DI
    CMP DI,Y2
    JNZ SquareLoop
    PopALL
ENDM DrawSquarePixel

ClearString Macro STRING,Size
    Local ZLOOPO
    PushALL
    Mov SI,Offset STRING+2
    Mov CX,Size
    ZLOOPO:
    Mov Al,SDollar
    Mov [SI],Al
    INC SI
    DEC CX
    JNZ ZLOOPO
    POPALL
ENDM ClearString

MakeCapital Macro STRING,Size
    Local M,BIG
    PushALL
    Mov SI,Offset STRING+2
    Mov CX,Size
    M:
    Mov AH,[SI]
    CMP AH,61H
    JB BIG
    CMP Ah,7AH
    JA BIG
    SUB AH,20H
    BIG:
    Mov [SI],AH
    INC SI
    DEC CX
    JNZ M
    POPALL
ENDM MakeCapital

ReverseCapital Macro STRING,Size
    Local M,BIG
    PushALL
    Mov SI,Offset STRING+2
    Mov CX,Size
    M:
    Mov AH,[SI]
    CMP AH,61H
    JC BIG
    Add AH,20H
    BIG:
    Mov [SI],AH
    INC SI
    DEC CX
    JNZ M
    POPALL
ENDM ReverseCapital

FCharacterCheck1 Macro STR,Size,CharFound  ;To check (if present) forbidden character of P1
    Local X
    mov AX,DS 
    mov ES,AX
    Mov CharFound,0
    Mov DI,offset STR + 2
    Mov AL, P2char
    Mov Cx, Size
    REPNE SCASB 
    CMP CL,0
    JZ X 
    Mov CharFound,1
    X:
ENDM FCharacterCheck1

FCharacterCheck2 Macro STR,Size,CharFound ;To check (if present) forbidden character of P2
    Local X
    mov AX,DS 
    mov ES,AX
    Mov CharFound,0
    Mov DI,offset STR + 2
    MakeCapital P1Char,1
    Mov AL, P1char
    Mov Cx, Size
    REPNE SCASB 
    CMP CL,0
    JZ X 
    Mov CharFound,1
    X:
ENDM FCharacterCheck2

HexaToDecimal Macro InputNumber,DispNumber
    Local LOOPX,Num,Continue
        PushALL
        Push DI
        ClearString DispNumber,4
        mov ax, InputNumber
        Mov DI,offset DispNumber
        Mov BX,10
        mov cx, 0
        LOOPX:
        Mov DX,0
        Div BX 
        push dx
        inc cx
        cmp ax,0
        jnz loopx

        Continue:
        pop dx
        add dx,30h
        mov [di], dl
        inc di
        DEC cx
        JNZ Continue
        pop DI
        PopALL
ENDM HexaToDecimal


.DATA
;First Screen
NameX db 'Please enter your Name: ','$'
Points db 'Enter your points: ','$'
Continue db 'Press any key to continue: ','$'
Spam db 2,?,2 dup('$')
XPoint EQU 20

;First Player
P1Name db 30,?,30 dup('$')
P1Points db 5,?,5 dup('$')

;Second Player
P2Name db 30,?,30 dup('$')
P2Points db 5,?,5 dup('$')



;Second Screen (Main Screen)
PlayMessege db 'Your other party is: ','$'
ChatInvitationRecieved db '- Chat invitation recieved (F1) ','$'  
GameInvitationRecieved db '- Game invitation recieved (F2) ','$'

ChatInvitationSent db '- You sent a chat invitation','$'  
GameInvitationSent db '- You sent a Game invitation','$'

Chat db 'To start Chatting press F1','$'
Game db 'To start the Game F2','$'
EndProgram db 'To End the program press ESC','$'

ChoiceP1 db ?
ChoiceP2 db ? 
Flag db 0

;Third Screen (Chat Mode)
P1Chat db 50,?,50 dup('$')
P2Chat db 50,?,50 dup('$')
Chat_2 db 50,?,50 dup('$')
SDollar db '$'


;;GAME MODE SCREEENS;;
;CHOSE POINTS SCREEN:
    InitialPoints db 'Enter initial points ?','$'
    P1_Game_Points db 5,?,5 dup('$')
    P2_Game_Points db 5,?,5 dup('$')
    P1_Score DW ?
    P2_Score DW ?
    P1_ScoreP DB 5 dup('$')
    P2_ScoreP DB 5 dup('$')
   

;Choose Level Screen
    X dW ?
    ChooseLevel db 'C','h','o','o','s','e','_','t','h','e','_','l','e','v','e','l','?'
    Level db ?
    Loading db 'L','O','A','D','I','N','G'

;Choose forbidden character screen
    ChosenLevel db 'Level:' ,'$'
    ChooseCharacter1 db 'Choose forbidden character','$'
    P1char db ?
    P2char db ?

;Game Main Game Screen
Target dw 105EH
Winner db ?
Scores db 'Score','$'
CharF db 0
ToggleFlag db 0
Case db 0
no2teten db ':'

R1  db 'A','X'
    db 'B','X'
    db 'C','X'
    db 'D','X'
    db 'S','I'
    db 'D','I'
    db 'S','P'
    db 'B','P'

    Trial db 5 dup('$')
    MTrial db 3 dup('$')
    P1Memory    DB 12H, 34H, 10H, 00, 0A0H, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00
    P2Memory    DB 00, 90H, 66H, 00, 80H, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00
    P1Registers dw 5F11H ;P1AX
                dw 1255H ;P1BX
                dw 1455H ;P1CX
                dw 1800H ;P1DX
                dw 1452H ;P1SI
                dw 1999H ;P1DI
                dw 2000H ;P1SP
                dw 9911H ;P1BP

    P2Registers dw 1111H ;P2AX
                dw 1255H ;P2BX
                dw 1455H ;P2CX
                dw 1800H ;P2DX
                dw 1452H ;P2SI
                dw 1999H ;P2DI
                dw 2000H ;P2SP
                dw 9911H ;P2BP


    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    P1Command db 7,?,7  dup('$')
    P2Command db 7,?,7  dup('$') 
    CommandNumber db ?

    P1Source db 7,?,7  dup('$')
    P2Source db 7,?,7  dup('$') 
    SourceNumber dw ?

    P1Destination db 5,?,5  dup('$') 
    P2Destination db 5,?,5  dup('$')  
    DestinationNumber dw ?    

    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;Commands
        ComMOV db 'MOV'
        ComADD db 'ADD'
        ComADC db 'ADC'
        ComSUB db 'SUB'
        ComSBB db 'SBB'
        ComDIV db 'DIV'
        ComMUL db 'MUL'
        ComXOR db 'XOR'
        ComAND db 'AND'
        ComNOP db 'NOP'
        ComSHR db 'SHR'
        ComSHL db 'SHL'
        ComSAR db 'SAR'
        ComCLC db 'CLC'
        ComROR db 'ROR'
        ComRCL db 'RCL'
        ComRCR db 'RCR'
        ComROL db 'ROL'
        ComINC db 'INC'
        ComDEC db 'DEC'
        ComOR db 'OR'
        ComPush db 'PUSH'
        ;Registers
        RAX db 'AX'
        RBX db 'BX'
        RCX db 'CX'
        RDX db 'DX'
        RSI db 'SI'
        RDI db 'DI'
        RSP db 'SP'
        RBP db 'BP'
        RAL db 'AL'
        RAH db 'AH'
        RBL db 'BL'
        RBH db 'BH'
        RCLL db 'CL'
        RCH db 'CH'
        RDL db 'DL'
        RDH db 'DH'
        SBracket db '['
        ;MEMORY ADDRESSES
        Address db '0','1','2','3','4','5','6','7','8','9','A','B','C','D','E','F'
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;POWER UPS
    PorC db ?
    PowerUp db ?
    P1Stock db 1, 1, 1
    P2Stock db 1, 1, 1
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
NumberHolder dw ? 
ChooseRegister db ?
Initially db 0

;Sores Screen
PWinner db 'W','I','N','N','E','R'

;Phase2
Val db ?

;Shooting
mai db ?
x1 dw ?
y1 dw ?
x2 dw ?
y2 dw ?
x3 dw ?;yellow object starting point
x4 dw ?;yellow object starting point
x5 dw ?;red object starting point
x6 dw ?;red object starting point
x7 dw ?;green object starting point
x8 dw ?;green object starting point
yay dw ?
Shooted dw 0



;Phase2
Value db ?,'$'
;ENDSCREEN
FINISHED db 'Game Stopped','$'



.CODE
;..................................
;           First Screen          ;
;..................................
JMP Far Ptr Main
FirstS PROC
   ReEnterN2:  Clear
        
    ;For Displaying Lines
    MoveCursor 20D,5D 
    DisplayString NameX
    NewFLine XPoint,4
    DisplayString Points
    NewFLine XPoint,4
    DisplayString Continue
    
    ;For Reading lines                                           
        MoveCursor 20D,7D 
        ReadString P2Name
        CMP P2Name+1, 15D
        JB C21
        JMP ReEnterN2
        C21:
        CMP P2Name+2 , 41H
        JA C22
        JMP ReEnterN2
        C22:
        CMP P2Name+2 , 7AH
        JB C23
        JMP ReEnterN2
        C23:

    NewFLine XPoint,4
    ReadString P2Points
    NewFLine XPoint,4
    ReadString Spam

    ;Send Name 
   STRINGCOPY P2Name,Chat_2,30
   Mov Cx,30
   Call Send
   ;Send Points
   STRINGCOPY P2Points,Chat_2,5
   Mov Cx,5
   Call Send

   ;Recieve Name
   Mov Cx,30
   Call recieve
   STRINGCOPY Chat_2,P1Name,30

   ;Recieve Points
   Mov Cx,5
   Call recieve
   STRINGCOPY Chat_2,P1Points,5

    RET       
FirstS ENDP

;..................................
;          Second Screen          ;
;.................................. 
SecondS Proc
    Clear
    Mov Ah,0
    Mov Al,03h
    INT 10H
    ;Designing the screen 

    ;Drawing a horizontal line at the bottom of the screen (Notification Bars)
    Mov Cl,80D
    Mov Bl,0
    HLine: 
    MoveCursor Bl,20
        Mov Ah,2
        Mov Dl, '-'
        int 21H
    INC Bl
    DEC Cl
    JNZ HLine

    ;Printing each other names 
    MoveCursor 0,0
    DisplayString PlayMessege
    DisplayStringRead P1Name  

    ;Priting the Main for both Screens 
        MoveCursor 16,8
        DisplayString Chat
        NewFline 16,2 
        DisplayString Game
        NewFline 16,2
        DisplayString EndProgram
        NewFline 16,2


    Mov ChoiceP1,0
    Mov ChoiceP2,0

    Player1:
    call recChar
    Mov Al,Val
    Mov ChoiceP1,Al


    ;Choices are Stored in (ChoiceP1)
    ;1) If choose ESC with Scan Code 01
    Mov Al,01
    CMP ChoiceP1,Al
    JNZ NoExit
    JMP Ex
    NoExit:

    ;2) If choose F1 with Scan Code 3D  
    Mov Al,3DH
    CMP ChoiceP1,Al
    JZ P1StartChat


    ;3) If choose F2 with Scan Code 3C
    Mov Al,3CH
    CMP ChoiceP1,Al
    JZ P1StartGame  
        

    P1StartChat:
    ;Check if P2 Invited Go to Chat
    Mov Al,ChoiceP1
    CMP Al,ChoiceP2
    JZ Chatting 

    Mov Flag,0
    ;Messege in PLAYER2 Notification that invitation is recieved (No player writes recieve to himself)
    MoveCursor 0,21
    DisplayString ChatInvitationRecieved
    JMP Player2

    P1StartGame:
    ;Check if P2 Invited Go to Game
    Mov Al,ChoiceP1
    CMP Al,ChoiceP2
    JZ PlayGame 

    Mov Flag,0
    ;Messege in PLAYER2 Notification that invitation is recieved (No player writes recieve to himself)
    MoveCursor 0,21
    DisplayString GameInvitationRecieved
    JMP Player2  

    PlayGame: Call GameMode
    Chatting: Call ChatMode
    Ex: Call BYE

    Player2:
    Mov Ah,0h
    int 16H
    Mov ChoiceP2,AH
    Mov Val,Ah
    Call sendChar 

    ;SCROLL THE SCREEN
    PushALL
        mov ax,0701h 
        mov bh,07 
        mov cx,1500H 
        mov dx,1627H
        int 10H

        mov ax,0701h 
        mov bh,07 
        mov cx,1529H 
        mov dx,1650H
        int 10H
    PopAll

    ;Choices are Stored in (ChoiceP2)
    ;1) If choose ESC with Scan Code 01
    Mov Al,01
    CMP ChoiceP2,Al
    JZ Ex

    ;2) If choose F1 with Scan Code 3D  
    Mov Al,3DH
    CMP ChoiceP2,Al
    JZ P2StartChat


    ;3) If any user choose F2 with Scan Code 3C
    Mov Al,3CH
    CMP ChoiceP2,Al
    JZ P2StartGame  
    

    P2StartGame:
    ;Check if P2 Invited Go to Game
    Mov Al,ChoiceP1
    CMP Al,ChoiceP2
    JZ PlayGame 

    Mov Flag,1
    ;Messege in PLAYER2 Notification that invitation is recieved (No player writes recieve to himself)
    MoveCursor 0,21
    DisplayString GameInvitationSent
    JMP Player1   

    P2StartChat:
    ;Check if P2 Invited Go to Chat
    Mov Al,ChoiceP1
    CMP Al,ChoiceP2
    JNZ NoChatting
    JMP Chatting 
    NoChatting:
    Mov Flag,1
    ;Messege in PLAYER2 Notification that invitation is recieved (No player writes recieve to himself)
    MoveCursor 0,21
    DisplayString ChatInvitationSent
    JMP Player1 
    RET
SecondS ENDP

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;         Shooting Game           ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  
ShootingS Proc
    ;Designing the mini game screen
    ;Change to video mode

    mov x3,6 ;yellow object starting point
    mov x4,21 ;yellow object starting point
    DrawSquarePixel x3,x4,0,15,0Eh ;yellow flying object of width 15
    

    mov x5,15 ;red object starting point
    mov x6,23 ;red object starting point
    DrawSquarePixel x5,x6,0,8,4 ;red flying object of width 17
    
    mov x7,20 ;green object starting point
    mov x8,32 ;green object starting point
    DrawSquarePixel x7,x8,0,12,2 ;green flying object of width 12

    mov x1, 0;blue object starting point
    mov x2, 10;blue object starting point
    mov cx,1000
    DrawSquarePixel x1,x2,0,10,9 ;blue flying object of width 10

    DrawSquarePixel 158,168,165,175,9 ; gun

    mov y1,168
    mov y2,172
    mov cx,1000
    mov dx, 170
    DrawSquarePixel 161,165,y1,y2,0FH ; bullet
    
    LOOPFLying2: ;loop of flying object till space is pressed
    DrawSquarePixel x1,x2,0,10,0
    add x1, 10
    add x2, 10
    DrawSquarePixel x1,x2,0,10,9
    DrawSquarePixel x3,x4,0,15,0
    add x3,12
    add x4,12
    DrawSquarePixel x3,x4,0,15,0Eh
    DrawSquarePixel x5,x6,0,8,0
    add x5,15
    add x6,15
    DrawSquarePixel x5,x6,0,8,4
    DrawSquarePixel x7,x8,0,12,0
    add x7,17
    add x8,17
    DrawSquarePixel x7,x8,0,12,2
    dec cx
    hlt
    hlt
    mov ah,1
    int 16H
    cmp ah,57
    jz LoopFlying1 
    hlt
    hlt
    jmp LOOPFLying2

    LOOPFLying1:
    DrawSquarePixel 161,165,y1,y2,0
    sub y1, 10
    sub y2, 10
    DrawSquarePixel 161,165,y1,y2,0FH
    dec cx
    hlt
    DrawSquarePixel x1,x2,0,10,0
    add x1, 10
    add x2, 10
    DrawSquarePixel x1,x2,0,10,9
    DrawSquarePixel x3,x4,0,15,0
    add x3,10
    add x4,10
    DrawSquarePixel x3,x4,0,15,0Eh
    DrawSquarePixel x5,x6,0,8,0
    add x5,10
    add x6,10
    DrawSquarePixel x5,x6,0,8,4
    DrawSquarePixel x7,x8,0,12,0
    add x7,10
    add x8,10
    DrawSquarePixel x7,x8,0,12,2
    dec cx
    sub dx,10
    hlt
    hlt
    cmp dx,10
    jna getcolor
    jmp far ptr LoopFlying1

    getcolor:
    mov ah,0dh
    mov bh,0
    mov cx, 163
    mov dx, 4
    int 10h 
    cmp al,0    ;black=no hit
    jz etla3barra
    win:        ;red 4   yellow 3   green 5   blue 2
    cmp al, 2
    jz green
    cmp al, 4
    jz red
    cmp al, 0Eh
    jz yellow
    cmp al,9
    jz blue 
    
    
    etla3barra:
    mov Shooted,0
    RET

    green: 
    Mov Shooted,5h
    Add P2_Score,5
    ret

    red: 
    Mov Shooted,4h
    Add P2_Score,4
    ret

    yellow: 
    Mov Shooted,3h
    Add P2_Score,3
    ret

    blue: 
    Mov Shooted,2h
    Add P2_Score,2
    ret  

ShootingS ENDP
;..................................
;              EXIT               ;
;..................................  

EXIT PROC
    ReadString Spam
    clear
    MoveCursor 10,10
    DisplayString FINISHED
    ReadString Spam
    Call SecondS
EXIT ENDP



;..................................
;           Game Screen           ;
;.................................. 
 
;Calling different screens like the Main Procedure
GameMode Proc
    Clear
    Call GInitialPoints
    Clear
    Call GLevel
    Clear
    Call GFCharacter
    ;Clear
    ;For Clearing All registers
        PushALL
        Mov Bl,1
        Call ClearALLRegisters
        PopAll
    Call GMainScreen
GameMode ENDP

;1]] Choose Initial Points
GInitialPoints Proc

    Mov P1_Score,0
    Mov P2_Score,0
    ClearString P1_ScoreP,5
    ClearString P2_ScoreP,5
    ClearString P1_Game_Points,5
    ClearString P2_Game_Points,5
    ClearString Chat_2,30
    ;Priting the Main for both Screens 
        MoveCursor 25,8
        DisplayString InitialPoints

        MoveCursor 25,8
        NewFLine 25,2
        ReadString P2_Game_Points
        
        Mov Cx,5
        STRINGCOPY P2_Game_Points,Chat_2,5
        Call send

        Push Cx
        Mov Ch,0
        Mov Cl,P2_Game_Points+1   
        Mov Val,Cl
        call sendChar
        Pop Cx

        Mov Cx,5
        Call recieve
        STRINGCOPY Chat_2,P1_Game_Points,5

        Push Cx
        Call recChar
        Mov Ch,0
        Mov Cl,Val
        Mov P1_Game_Points+1,Cl
        Pop Cx

    ;To Covert Player 1 Points
        Push SI
        Push DI 
        StringToNumber P1_Game_Points
        STRINGCOPY P1_Game_Points ,P2Source,5 
        POP DI
        POP SI
        Call DecNumberIntoBX
        Mov AX,NumberHolder
        MOV P1_Score, AX

        ;To Convert Player2 Points
        Push SI
        Push DI
        StringToNumber P2_Game_Points      ;31 30 30 --> 1 0 0              7,3,1,0,0
        STRINGCOPY P2_Game_Points ,P2Source,5
        POP DI
        POP SI
        Call DecNumberIntoBX
        Mov AX,NumberHolder
        MOV P2_Score, AX
        
        ;To Compare
        Mov Ax,P1_Score
        Mov Bx,P2_Score

        CMP Ax,Bx
        JC MinP1
        Mov P1_Score,Bx
        RET
        MinP1:
        Mov P2_Score,Ax
        RET
GInitialPoints ENDP

;;2]] ChooseLevel
GLevel Proc
    ;Change to video mode
    mov ah,0
    mov bh,0 
    mov al,13h
    int 10h

    mov ax, 1003h
    mov bx, 0
    int 10h


    CMP Flag,1
    JNZ P1_Started
    JMP P2_Started
    P1_Started:
    PushALL
    Mov SI,Offset Loading
    DisplayCharacterSTR 040H,16,10,7
    DrawHLinePixel 90,210,100,35H
    DrawHLinePixel 110,190,110,34H
    DrawHLinePixel 130,170,120,33H
    PopAll

    call recChar
    Push CX
    Mov Ch,0
    Mov Cl,Val
    Mov Level,Cl
    Pop Cx

    ;Validation Choose only level 1 or 2
    CMP Level,'1'
    JNZ CheckP2Level
    JMP ENDLevel
    CheckP2Level:
    CMP Level,'2'
    JNZ WrongLevel
    JMP ENDLevel
    WrongLevel: Call GLevel
    RET

    P2_Started:
    Mov SI,Offset ChooseLevel
    DisplayCharacterSTR 050H,12,8,17
    MoveCursor 20,10
    ReadCharacter Level
    
    ;Validation Choose only level 1 or 2
    CMP Level,'1'
    JNZ CheckP2Level1
    JMP SENDLevel
    CheckP2Level1:
    CMP Level,'2'
    JNZ WrongLevel1
    JMP SENDLevel
    WrongLevel1: Call GLevel
    SENDLevel:
    Push CX
    Mov Ch,0
    Mov Cl,Level
    Mov Val,Cl
    call sendChar 
    Pop Cx
    ENDLevel:RET
GLevel ENDP

;;3]] ChooseCharacter
GFCharacter  PROC
    ;Change to video mode
    mov ah,0
    mov bh,0 
    mov al,13h
    int 10h

    mov ax, 1003h
    mov bx, 0
    int 10h


    CMP Flag,0
    JZ P1Control
    JMP ChooseYourCharacter

    P1Control: 
    MoveCursor 1,1
    DisplayString ChosenLevel
    MoveCursor 8,1
    DisplayCharacter Level, 20H

    ChooseYourCharacter:
    ;Choosing forbidden character
    MoveCursor 7,5
    DisplayString ChooseCharacter1

    call recChar
    Push CX
    Mov Ch,0
    Mov Cl,Val
    Mov P1Char,Cl
    Pop Cx
    

    ;Input Forbidden character
    MoveCursor 20,8
    ReadCharacter P2char
    Push CX
    Mov Ch,0
    Mov Cl,P2Char
    Mov Val,Cl
    call sendChar 
    Pop Cx

        ;Change to video mode
        mov ah,0
        mov bh,0 
        mov al,13h
        int 10h

        mov ax, 1003h
        mov bx, 0
        int 10h

    RET
GFCharacter  ENDP

;4]] Game Screen 
GMainScreen Proc
    Mov ToggleFlag,0
    ;Check if Target Value is found
    Call CheckTarget
    CMP Winner,1
    JB NoWinner
    Call ShowScore

    NoWinner:
    ;PushALL
    ;Push SI
    ;Push DI
    ;DrawSquarePixel 0,360,0,178,0FFH
    ;Pop DI
    ;Pop SI
    ;PopAll
    ;Change to video mode
        mov ah,0
        mov bh,0 
        mov al,13h
        int 10h

        mov ax, 1003h
        mov bx, 0
        int 10h

    Mov SI, Offset P1Registers
    Mov Ax,SI
    CMP Ax,100
    JNZ No
    Call ShootingS

    Mov SI, Offset P2Registers+2
    Mov Ax,SI
    CMP Ax,100
    JNZ No
    Call ShootingS


    No:   
    ;For Screen Splitting
    DrawVLinePixel 161D,0D,180D,0EH
    DrawVLinePixel 162D,0D,180D,0EH
    DrawVLinePixel 163D,0D,180D,0EH
    DrawHLinePixel 0D,320D,180,0EH  

    ;For Memory Splitting
    DrawVLinePixel 140D,5D,140D,0EH
    DrawVLinePixel 185D,5D,140D,0EH
    DrawHLinePixel 140D,185D,5D,0EH  
    DrawHLinePixel 140D,185D,140D,0EH  

    ;Displaying Memory Values  ;Works by moving SI
    ;For Printing P1
    Mov CL, 16D
    Mov SI, offset P1Memory
    MoveCursor 18,1
    Printing:
    MemoryToString MTrial
    DisplayString MTrial
    NewFLine  18,1 
    INC SI
    DEC CL 
    JNZ Printing


    ;Displaying Memory Values  ;Works by moving SI
    ;For Printing P2
    Mov CL, 16D
    Mov SI, offset P2Memory
    MoveCursor 21,1
    Printing1:
    MemoryToString MTrial
    DisplayString MTrial
    NewFLine  21,1 
    INC SI
    DEC CL 
    JNZ Printing1

    ;PRINTING MEMORY ADDRESSES FOR P1
    MoveCursor 16,1
    DisplayCharacter Address,20H
    NewFLine 16,1
    DisplayCharacter Address+1,20H
    NewFLine 16,1
    DisplayCharacter Address+2,20H
    NewFLine 16,1
    DisplayCharacter Address+3,20H
    NewFLine 16,1
    DisplayCharacter Address+4,20H
    NewFLine 16,1
    DisplayCharacter Address+5,20H
    NewFLine 16,1
    DisplayCharacter Address+6,20H
    NewFLine 16,1
    DisplayCharacter Address+7,20H
    NewFLine 16,1
    DisplayCharacter Address+8,20H
    NewFLine 16,1
    DisplayCharacter Address+9,20H
    NewFLine 16,1
    DisplayCharacter Address+10,20H
    NewFLine 16,1
    DisplayCharacter Address+11,20H
    NewFLine 16,1
    DisplayCharacter Address+12,20H
    NewFLine 16,1
    DisplayCharacter Address+13,20H
    NewFLine 16,1
    DisplayCharacter Address+14,20H
    NewFLine 16,1
    DisplayCharacter Address+15,20H
    NewFLine 16,1

    ;PRINTING MEMORY ADDRESSES FOR P1
    MoveCursor 24,1
    DisplayCharacter Address,20H
    NewFLine 24,1
    DisplayCharacter Address+1,20H
    NewFLine 24,1
    DisplayCharacter Address+2,20H
    NewFLine 24,1
    DisplayCharacter Address+3,20H
    NewFLine 24,1
    DisplayCharacter Address+4,20H
    NewFLine 24,1
    DisplayCharacter Address+5,20H
    NewFLine 24,1
    DisplayCharacter Address+6,20H
    NewFLine 24,1
    DisplayCharacter Address+7,20H
    NewFLine 24,1
    DisplayCharacter Address+8,20H
    NewFLine 24,1
    DisplayCharacter Address+9,20H
    NewFLine 24,1
    DisplayCharacter Address+10,20H
    NewFLine 24,1
    DisplayCharacter Address+11,20H
    NewFLine 24,1
    DisplayCharacter Address+12,20H
    NewFLine 24,1
    DisplayCharacter Address+13,20H
    NewFLine 24,1
    DisplayCharacter Address+14,20H
    NewFLine 24,1
    DisplayCharacter Address+15,20H
    NewFLine 24,1


    ;For Displaying the User Names
    DrawHLinePixel 0,320,160,0EH
    MoveCursor 1,21
    DisplayStringRead P1Name
    MoveCursor 21,21
    DisplayStringRead P2Name

    ;For Displaying the User Names In Game Chat
    MoveCursor 1,23
    DisplayStringRead P2Name
    MoveCursor 7,23
    DisplayCharacter no2teten, 0FH
    MoveCursor 1,24
    DisplayStringRead P1Name
    MoveCursor 7,24
    DisplayCharacter no2teten, 0FH

    ;For Displaying the Score
    MoveCursor 8,21
    DisplayString Scores
    MoveCursor 29,21
    DisplayString Scores


    CMP Level,'2'
    JZ DoNotShowF
    ;For Displaying the Forbidden Character
    MoveCursor 1,0
    DisplayCharacter P2Char,33H
    MoveCursor 38,0
    DisplayCharacter P1Char,33H

    DoNotShowF:
    HexaToDecimal P1_Score, P1_ScoreP 
    MoveCursor 14,21 
    DisplayString P1_ScoreP

    HexaToDecimal P2_Score, P2_ScoreP 
    MoveCursor 35,21 
    DisplayString P2_ScoreP


    ;Displaying Register !!!!Names FOR PLAYER 1!!!!!
    Mov SI,Offset R1
    DisplayCharacterSTR 040,4,1,2
    DisplayCharacterSTR 040,4,5,2
    DisplayCharacterSTR 040,4,9,2
    DisplayCharacterSTR 040,4,13,2
    DisplayCharacterSTR 040,10,1,2
    DisplayCharacterSTR 040,10,5,2
    DisplayCharacterSTR 040,10,9,2
    DisplayCharacterSTR 040,10,13,2

    ;FOR Initializing Registers Player1
    CMP Level,'2'
    JZ Level2Initialize1
    JMP NoInitialization1

        Level2Initialize1:
        CMP Initially,0
        JZ Init1
        JMP NoInitialization1
        Init1:
        ;For the first player in Level (2)
        Mov Cl,8
        Mov DI,0
        Mov Initially,1
        InitializeP1:
        PushALL
        MoveCursor 4,18
        ;ReadString P2Source
        PUSH CX
        PUSH AX
        Mov Cx,7
        Call recieve
        STRINGCOPY Chat_2,P2Source,7
        Call recChar
        MOV AL, Val
        MOV P2Source+1, AL
        POP AX
        POP CX
        StringToNumber P2Source
        Call NumberIntoBX
        Mov Bx,NumberHolder ;BX Contains the Number
        CMP BX,Target       ;Check if target is present (Mov Zero's)
        JNZ TargetNotFound
        Mov BX,0
        TargetNotFound:
        Mov SI,offset P1Registers
        Add SI,DI
        Mov [SI],BX
        Add DI,2
        Push SI
        Push DI
        DrawSquarePixel 20,100,130,155,0FFH
        Pop DI
        Pop SI
        PopALL
        DEC CL
        JZ NoInitialization1
        JMP InitializeP1
        
    NoInitialization1:
    MoveCursor 3,2
    NumberToString P1Registers,Trial    ;Printing AX
    DisplayString Trial
    NewFLine  3,4 
    NumberToString P1Registers+2,Trial  ;Printing BX
    DisplayString Trial 
    NewFLine  3,4 
    NumberToString P1Registers+4,Trial  ;Printing CX
    DisplayString Trial
    NewFLine  3,4 
    NumberToString P1Registers+6,Trial  ;Printing DX
    DisplayString Trial

    MoveCursor 9,2
    NumberToString P1Registers+8,Trial  ;Printing SI
    DisplayString Trial
    NewFLine  9,4 
    NumberToString P1Registers+10,Trial ;Printing DI
    DisplayString Trial
    NewFLine  9,4 
    NumberToString P1Registers+12,Trial ;Printing SP
    DisplayString Trial
    NewFLine  9,4 
    NumberToString P1Registers+14,Trial ;Printing BP
    DisplayString Trial




    ;Displaying Register !!!!Names FOR PLAYER 2!!!!!
    Mov SI,Offset R1
    DisplayCharacterSTR 040,28,1,2
    DisplayCharacterSTR 040,28,5,2
    DisplayCharacterSTR 040,28,9,2
    DisplayCharacterSTR 040,28,13,2
    DisplayCharacterSTR 040,34,1,2
    DisplayCharacterSTR 040,34,5,2
    DisplayCharacterSTR 040,34,9,2
    DisplayCharacterSTR 040,34,13,2

    ;FOR Initializing Registers Player1
        CMP Level,'2'
        JZ Level2Initialize2
        JMP NoInitialization2

        Level2Initialize2:
        CMP Initially,1
        JZ Init2
        JMP NoInitialization2
        Init2:
        ;For the first player in Level (2)
        Mov Cl,8
        Mov DI,0
        Mov Initially,2
        InitializeP2:
        PushALL
        MoveCursor 27,18
        ReadString P2Source
        Push CX
        PUSH AX
        STRINGCOPY P2Source,Chat_2,7
        Mov Cx,7
        Call send
        Mov AL, P2Source+1
        Mov Val, AL
        Call sendChar
        POP AX
        POP Cx
        StringToNumber P2Source
        Call NumberIntoBX
        Mov Bx,NumberHolder ;BX Contains the Number
        CMP BX,Target       ;Check if target is present (Mov Zero's)
        JNZ TargetNotFound0
        Mov BX,0
        TargetNotFound0:
        Mov SI,offset P2Registers
        Add SI,DI
        Mov [SI],BX
        Add DI,2
        Push SI
        Push DI
        DrawSquarePixel 200,260,130,155,0FFH
        Pop DI
        Pop SI
        PopALL
        DEC CL
        JZ NoInitialization2
        JMP InitializeP2
        
    NoInitialization2:
    MoveCursor 27,2
    NumberToString P2Registers,Trial    ;Printing AX
    DisplayString Trial
    NewFLine  27,4 
    NumberToString P2Registers+2,Trial  ;Printing BX
    DisplayString Trial 
    NewFLine  27,4 
    NumberToString P2Registers+4,Trial  ;Printing CX
    DisplayString Trial
    NewFLine  27,4 
    NumberToString P2Registers+6,Trial  ;Printing DX
    DisplayString Trial

    MoveCursor 33,2
    NumberToString P2Registers+8,Trial  ;Printing SI
    DisplayString Trial
    NewFLine  33,4 
    NumberToString P2Registers+10,Trial ;Printing DI
    DisplayString Trial
    NewFLine  33,4 
    NumberToString P2Registers+12,Trial ;Printing SP
    DisplayString Trial
    NewFLine  33,4 
    NumberToString P2Registers+14,Trial ;Printing BP
    DisplayString Trial

    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]
    CMP Flag,1
    JZ P2Action
    JMP Player1Game
    P2Action: JMP FAR PTR Player2Game

    Player1Game:
    DrawHLinePixel 20,110,140,050H
    DrawHLinePixel 20,110,155,050H
    DrawVLinePixel 20,140,155,050H
    DrawVLinePixel 110,140,155,050H

    DrawHLinePixel 1,19,140,060H
    DrawHLinePixel 1,19,155,060H
    DrawVLinePixel 1,140,155,060H
    DrawVLinePixel 19,140,155,060H
    WrongInput1: 
    ;ReadCharacter PorC     ;P=>1)Clear 2)Change_Forbidden_Character 3)Command    C=>Command
    Call recChar
    PUSH CX
    MOV CH,0
    MOV CL, VAl
    MOV PorC, CL
    POP CX
    MoveCursor 1,18
    DisplayCharacter PorC, 0FH
    ;IF P (Read the Power up character)
    CMP PorC,'E'  ;Scancode of F4
    JNZ NoMainMenu
    JMP Far ptr SecondS  ;Call the main menue
    NoMainMenu:
    CMP PorC,'P'
    JZ Power1
    CMP PorC, 'p'
    JZ Power1
    CMP PorC, 'C'
    JNZ Talk1
    JMP C_1
    Talk1:
    CMP PorC, 'T'
    JNZ WrongInput1
    JMP InLineChat1

    Power1:
    ;MoveCursor 1,18
    ;ReadCharacter PowerUp   ;If 1)Clear   2)Change_Forbidden_Character  3)Which_register_to_play_on
    Call recChar
    PUSH CX
    MOV CH,0
    MOV CL, VAl
    MOV PowerUp, CL
    POP CX
    MoveCursor 1,18
    DisplayCharacter PowerUp, 0FH


    CMP PowerUp,'1' ;Clear 30 points
    JNZ ChangeForbidden1
    CMP P1Stock,1 ;Check if power up is used before
    JZ P1Clear
    JMP C_1
        P1Clear:
        Mov BX,P1_Score
        CMP BX,1EH
        JAE P1SufficientC     ;Check if points are suifficient
        JMP C_1
        P1SufficientC:
            SUB BX,1EH
            MOV P1_Score,BX
            Mov P1Stock,0   ;To update Stock
            Mov Bx,0        ;To perform the clear
            MOV BL,2
            Call ClearALLRegisters
            Call ShootingS
            CMP Shooted,1
            JZ Increase1
            JMP C_1
            Increase1: Add P1_Score,5
            JMP C_1

    ChangeForbidden1:
    CMP PowerUp,'2' ;Forbidden  8 points
    JNZ ChangeOwnReg1
    CMP P1Stock+1,1 ;Check if power up is used before
    JZ P1Forbidden
    JMP C_1
        P1Forbidden:
        Mov BX,P1_Score
        CMP BX,8H
        JAE P1SufficientF     ;Check if points are suifficient
        JMP C_1
        P1SufficientF:
            SUB BX,8H
            MOV P1_Score,BX
            Mov P1Stock+1,0
            MoveCursor 1,18
            ;ReadCharacter P1Char
            call recChar
            Mov Al,Val
            Mov P1Char,Al
            JMP C_1

    ChangeOwnReg1:    ;  5 points     
    CMP PowerUp,'3'
    JNZ ChangeOnBoth1
    Mov BX,P1_Score
        CMP BX,5H
        JAE P1ChangeOwn     ;Check if points are suifficient
        JMP C_1
        P1ChangeOwn:
            SUB BX,5H
            MOV P1_Score,BX
            Mov ToggleFlag,1   ;To Change his own register
            JMP C_1

    ChangeOnBoth1:  ; 3 points
    CMP PowerUp,'4'
    JNZ ChangeTValue1
    Mov BX,P1_Score
        CMP BX,3H
        JAE P1ChangeBoth     ;Check if points are suifficient
        JMP C_1
        P1ChangeBoth:
            SUB BX,3H
            MOV P1_Score,BX
            Mov ToggleFlag,3

    ChangeTValue1:  
    CMP Level, '2'
    JZ COMP1 
    JMP C_1
    COMP1:
    CMP PowerUp,'5'
    JZ COMP2
    JMP C_1
    COMP2:
    CMP P1Stock+2,1 ;Check if power up is used before
        JZ COMP3
        JMP C_1
        COMP3:
            MOV P1Stock+2,0 
            PushALL
            MoveCursor 3,18
            ;ReadString P2Source
            Push SI
            Push DI
            Mov Cx,7
            call recieve
            STRINGCOPY Chat_2,P2Source,7
            Call recChar
            Mov Al,Val
            Mov P2Source+1,Al
            MoveCursor 3,18
            DrawSquarePixel 20,100,142,155,0FFH
            POP DI
            POP SI
            PopALL
            StringToNumber P2Source
            Call NumberIntoBX
            MOV BX, NumberHolder
            MOV Ax, Target
            MOV Target, BX
            Call CheckTarget
            CMP Winner, 0
            JNZ NoChange1
            Mov Flag,1
            Call GMainScreen

    NoChange1:
        MOV Target, AX
        MOV Winner, 0
        Mov Flag,1
        Call GMainScreen

    InLineChat1:
        ;DrawSquarePixel 80,360,191,200,0FFH
        Mov CX,50
        call recieve
        STRINGCOPY Chat_2,P1Chat,50
        MoveCursor 8,24
        DisplayStringRead P1Chat

    C_1:
    ClearString Chat_2,50
    ;ReadString P1Command
    Mov Cx,7
    call recieve
    STRINGCOPY Chat_2,P1Command,7
    ClearString Chat_2,50
    MoveCursor 3,18
    DisplayStringRead P1Command
    ;ReadString P1Destination
    Mov Cx,7
    call recieve
    STRINGCOPY Chat_2,P1Destination,7
    ClearString Chat_2,50
    MoveCursor 7,18
    DisplayStringRead P1Destination
    ;ReadString P1Source
    Mov Cx,7
    call recieve
    STRINGCOPY Chat_2,P1Source,7
    MoveCursor 10,18
    DisplayStringRead P1Source
    Call recChar
    Push CX
    Mov Ch,0
    Mov Cl,Val
    Mov P1Source+1,Cl
    Pop Cx

    Mov Flag,1
    CMP Level,'2'
    JNZ Here
    ;ReadCharacter ChooseRegister
    Call recChar
    Mov Al,Val
    Mov ChooseRegister,Al
    CMP ChooseRegister,'1'
    JNZ Here
    Mov ToggleFlag,1
    Here: JMP GameProcedure



    Player2Game:
    DrawHLinePixel 210,300,140,050H
    DrawHLinePixel 210,300,155,050H
    DrawVLinePixel 210,140,155,050H
    DrawVLinePixel 300,140,155,050H

    DrawHLinePixel 191,209,140,060H
    DrawHLinePixel 191,209,155,060H
    DrawVLinePixel 191,140,155,060H
    DrawVLinePixel 209,140,155,060H
    WrongInput2: MoveCursor 25,18
    ReadCharacter PorC
    PUSH AX
    MOV AH, 0
    Mov AL, PorC
    Mov Val, AL
    POP AX
    Call sendChar
    ;IF P (Read the Power up character)
    CMP PorC,'E'  ;Scancode of F4
    JNZ NoMainMenu1
    JMP Far ptr SecondS  ;Call the main menue
    NoMainMenu1:
    CMP PorC,'P'
    JZ Power2
    CMP PorC,'p'
    Jz Power2
    CMP PorC,'C'
    JNZ Talk2
    JMP C_2
    Talk2:
    CMP PorC, 'T'
    JNZ WrongInput2
    JMP InLineChat2

    Power2:
    ;MoveCursor 25,18
    ;ReadCharacter PowerUp   ;If 1)Clear   2)Change_Forbidden_Character  3)Which_register_to_play_on
    MoveCursor 25,18
    ReadCharacter PowerUp   ;If 1)Clear   2)Change_Forbidden_Character  3)Which_register_to_play_on
    PUSH CX
    MOV CH, 0
    Mov CL, PowerUp
    Mov Val, CL
    POP CX
    Call sendChar

    CMP PowerUp,'1' ;Clear 30 points
    JNZ ChangeForbidden2
    CMP P2Stock,1 ;Check if power up is used before
    JZ P2Clear
    JMP C_2
        P2Clear:
        Mov BX,P2_Score
        CMP BX,1EH
        JAE P2SufficientC     ;Check if points are suifficient
        JMP C_2
        P2SufficientC:
            SUB BX,1EH
            MOV P2_Score,BX
            Mov P2Stock,0   ;To update Stock
            Mov Bx,0        ;To perform the clear
            MOV BL,3
            Call ClearALLRegisters
            Call ShootingS
            CMP Shooted,1
            JZ Increase2
            JMP C_2
            Increase2: Add P2_Score,5
            JMP C_2

    ChangeForbidden2:
    CMP PowerUp,'2' ;Forbidden  8 points
    JNZ ChangeOwnReg2
    CMP P2Stock+1,1 ;Check if power up is used before
    JZ P2Forbidden
    JMP C_2
        P2Forbidden:
        Mov BX,P2_Score
        CMP BX,8H
        JAE P2SufficientF     ;Check if points are suifficient
        JMP C_2
        P2SufficientF:
            SUB BX,8H
            MOV P2_Score,BX
            Mov P2Stock+1,0
            MoveCursor 1,18
            ReadCharacter P2Char
            Mov Al,P2Char
            Mov Val,Al
            call sendChar
            JMP C_2

    ChangeOwnReg2:
    CMP PowerUp,'3'
    JNZ ChangeOnBoth2
    Mov BX,P2_Score
        CMP BX,5H
        JAE P2ChangeOwn     ;Check if points are suifficient
        JMP C_2
        P2ChangeOwn:
            SUB BX,5H
            MOV P2_Score,BX
            Mov ToggleFlag,2   ;To Change his own register
            JMP C_2

    ChangeOnBoth2:
    CMP PowerUp,'4'
    JNZ ChangeTValue2
    Mov BX,P2_Score
        CMP BX,3H
        JAE P2ChangeBoth     ;Check if points are suifficient
        JMP C_2
        P2ChangeBoth:
            SUB BX,3H
            MOV P2_Score,BX
            Mov ToggleFlag,3

    ChangeTValue2:  
    CMP Level, '2'
    JZ COMP11 
    JMP C_2
    COMP11:
    CMP PowerUp,'5'
    JZ COMP22
    JMP C_2
    COMP22:
        CMP P2Stock+2,1 ;Check if power up is used before
        JZ COMP33
        JMP C_2
        COMP33:
            MOV P2Stock+2,0 
            PushALL
            MoveCursor 27,18
            ReadString P1Source
            STRINGCOPY P1Source,Chat_2,7
            Mov Cx,7
            Call Send
            Mov Al,P1Source+1
            Mov Val,Al
            Call sendChar
            Push SI
            Push DI
            DrawSquarePixel 210,250,142,155,0FFH
            POP DI
            POP SI
            PopALL
            StringToNumber P1Source
            Call NumberIntoBX
            MOV BX, NumberHolder
            MOV Ax, Target
            MOV Target, BX
            Call CheckTarget
            CMP Winner, 0
            JNZ NoChange2
            Mov Flag,0
            Call GMainScreen

    NoChange2:
        MOV Target, AX
        MOV Winner, 0
        Mov Flag,0
        Call GMainScreen

    InLineChat2:
    ;DrawSquarePixel 80,360,181,190,0FFH
    ClearString P2Chat,50
    ClearString Chat_2,50
    MoveCursor 8,23
    ReadString P2Chat
    MoveCursor 8,23
    DisplayStringRead P2Chat
    STRINGCOPY P2Chat,Chat_2,50
    MOV CX,50
    Call send


    C_2:
    MoveCursor 27,18
    ReadString P2Command
    STRINGCOPY P2Command,Chat_2,7
    Mov Cx,7
    Call send

    MoveCursor 31,18
    ReadString P2Destination
    STRINGCOPY P2Destination,Chat_2,7
    Mov Cx,7
    Call send

    MoveCursor 34,18
    ReadString P2Source
    STRINGCOPY P2Source,Chat_2,7
    Mov Cx,7
    Call send
    Push Cx
    Mov Ch,0
    Mov Cl,P2Source+1   
    Mov Val,Cl
    call sendChar
    Pop Cx

    Mov Flag,0
    CMP Level,'2'
    JNZ Here0
    ReadCharacter ChooseRegister
    ReadCharacter ChooseRegister
    Mov Al,ChooseRegister
    Mov Val,Al
    Call sendChar
    CMP ChooseRegister,'1'
    JNZ Here0
    Mov ToggleFlag,2
    Here0: JMP GameProcedure

        GameProcedure:
        Call MakeALLCapital
        ;IFF Forrbiden Character Present
        CMP Flag,1   ;Flag=1 Player 1 Lessa me5alas
        JZ FP1
        JMP FP2

        FP1:
            FCharacterCheck1 P1Command,3,CharF
            CMP CharF,1
            JZ Back1
            FCharacterCheck1 P1Source,2,CharF
            CMP CharF,1
            Jz Back1
            FCharacterCheck1 P1Destination,2,CharF
            CMP CharF,1
            JZ Back1
            JMP NOFChar 
            Back1: Call GMainScreen
        FP2:
            FCharacterCheck2 P2Command,3,CharF
            CMP CharF,1
            JZ Back2
            FCharacterCheck2 P2Source,2,CharF
            CMP CharF,1
            Jz Back2
            JMP XX
            Back2: Call GMainScreen
            XX: FCharacterCheck2 P2Destination,2,CharF
            CMP CharF,1
            JZ Back2
            JMP NOFChar
            
        NOFChar:               ;If No Forbidden Char Found
        Call FindRegisterDestination  ;Destination number assigned
        Call FindRegisterSource       ;Source number assigned


        ;;;;;;;; Working Registers ;;;;;; FlagToggle=0    FlagToggle=1 Write on player1 registers   FlagToggle=2 Write on player2 registers
        CMP ToggleFlag,1           ;;;;;; FlagToggle=3 Write on both registers
        JZ ToP2
        CMP ToggleFlag,2
        JZ XP1
        CMP ToggleFlag,3
        JZ XP1

    CMP Flag,1
        JZ XP1
        ToP2:JMP YP2

             XP1:
            CMP DestinationNumber,29
            JNZ NotBracket
                CMP SourceNumber,29
                JNZ NotMemorytomemory
                Dec P1_Score
                JMP Far ptr GMainScreen
                NotMemorytomemory: JMP NoMismatch1
            NotBracket:
            CMP DestinationNumber,16
            JB S1
            INC case

            S1:
            CMP SourceNumber,29
            Jz NoMismatch1
            CMP SourceNumber,30
            Jz NoMismatch1
            CMP SourceNumber,31
            Jz NoMismatch1
            CMP SourceNumber,16  ;in when SN>16
            JB Mismatch1
            INC case
            
            

            Mismatch1:
            CMP case,1
            JNZ NoMismatch1
            MOV case,0
            DEC P1_Score
            Call GMainScreen

            NoMismatch1:
            MOV case,0
            CMP DestinationNumber,29
            JNZ DefaultDest
                StringToNumber P1Destination
                MOV AX,0
                Mov AL,P1Destination+3    ;7, size, [, 1, ]
                MOV SI, offset P2Memory
                ADD SI, AX
                MOV AX, [SI]
                JMP CONTI
                    DefaultDest:   ;AX or AL
                    CMP DestinationNumber,16
                    JB DefaultinAX
                    ;if In AL
                    Mov SI,Offset P2Registers
                    Mov Cx,DestinationNumber
                    Sub CX,16
                    ADD SI,CX
                    Mov Ah,0
                    MOV AL,[SI]
                    JMP CONTI
                    
                    DefaultinAX:
                    Mov SI,Offset P2Registers
                    ADD SI,DestinationNumber
                    MOV AX,[SI]

            CONTI:
            CMP SourceNumber,30   ;IF Value is present
            JNZ NoVal
            JMP ValueX
            
            NoVal:
            CMP SourceNumber,31
            JNZ CAddress
            JMP NoValueX

            CAddress:
            CMP SourceNumber,29
            JNZ Cont
            StringToNumber P1Source
            MOV AX, 0
            Mov AL,P1Source+3    ;7, size, [, 1, ]
            MOV DI, offset P2Memory
            ADD DI, AX
            MOV BX, [DI]
            JMP Fcommand


            Cont: ;IF AX or AL
            CMP SourceNumber,16
            JB DefaultSource
            ;IN AL
            Mov DI,Offset P2Registers
            Sub SourceNumber,16
            ADD DI,SourceNumber
            Mov Bh,0
            MOV Bl,[DI]
            JMP Fcommand

            DefaultSource:
            Mov DI,Offset P2Registers
            ADD DI,SourceNumber
            MOV BX,[DI]
            
            Fcommand:
            Call FindCommand 
            
            CMP DestinationNumber, 29
            JNZ DefaultDest1
            Mov SI,Offset P2Memory
            MOV CX,0
            Mov CL,P1Destination+3 
            ADD SI, CX
            MOV [SI],AX
            JMP  HERE3

            DefaultDest1:   ;Either AX or AL
            CMP DestinationNumber,16
            JB Dest2ByteR
            ;IF 1Byte register
            Mov SI,Offset P2Registers
            Sub DestinationNumber,16
            ADD SI,DestinationNumber
            Mov [SI],AL
            JMP Here3

            Dest2ByteR:
            Mov SI,Offset P2Registers
            ADD SI,DestinationNumber
            MOV [SI],AX

            HERE3:
            CMP ToggleFlag,3
            JNZ NoPowerUp
            STRINGCOPY P1Command,P2Command,3
            JMP YP2
            NoPowerUp: Call GMainScreen

            ValueX:
            PushALL
            Call NumberIntoBX
            PopALL
            Mov BX,NumberHolder
            Call FindCommand 
            Mov SI,Offset P2Registers
            ADD SI,DestinationNumber
            MOV [SI],AX
            CMP ToggleFlag,3
            JNZ NoPowerUp
            STRINGCOPY P1Command,P2Command,3
            JMP YP2
            Call GMainScreen

            NoValueX: 
            Mov Bx,0
            Call FindCommand 
            Mov SI,Offset P2Registers
            ADD SI,DestinationNumber
            MOV [SI],AX
            CMP ToggleFlag,3
            JNZ NoPowerUp
            STRINGCOPY P1Command,P2Command,3
            JMP YP2
            Call GMainScreen

        
            
        ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 

            YP2:
            CMP DestinationNumber,29
            JNZ NotBracket1
                CMP SourceNumber,29
                JNZ NotMemorytomemory1
                Dec P2_Score
                JMP Far ptr GMainScreen
                NotMemorytomemory1: JMP NoMismatch2
            NotBracket1:
            CMP DestinationNumber,16
            JB S2
            INC case

            S2:
            CMP SourceNumber,29
            Jz NoMismatch2
            CMP SourceNumber,30
            Jz NoMismatch2
            CMP SourceNumber,31
            Jz NoMismatch2
            CMP SourceNumber,16
            JB Mismatch2
            INC case
            
            Mismatch2:
            CMP case,1
            JNZ NoMismatch2
            MOV case,0
            DEC P2_Score
            Call GMainScreen

            NoMismatch2: ;If 2Bytes or 4Bytes Register
            MOV case,0
            CMP DestinationNumber,29
            JNZ DefaultDest2
                StringToNumber P2Destination
                MOV AX,0
                Mov AL,P2Destination+3    ;7, size, [, 1, ]
                MOV SI, offset P1Memory
                ADD SI, AX
                MOV AX, [SI]
                JMP Complete
                    DefaultDest2:   ;AX or AL
                    CMP DestinationNumber,16
                    JB DefaultinAXy
                    ;if In AL
                    Mov SI,Offset P1Registers
                    Mov Cx,DestinationNumber
                    Sub CX,16
                    ADD SI,CX
                    Mov Ah,0
                    MOV AL,[SI]
                    JMP Complete
                            
                    DefaultinAXy:
                    Mov SI,Offset P1Registers
                    ADD SI,DestinationNumber
                    MOV AX,[SI]
            

            Complete:
            CMP SourceNumber,30
            JNZ NoVal2
            JMP ValueY

            NoVal2:
            CMP SourceNumber,31
            JNZ CAddress2
            JMP NoValueY

            CAddress2:
            CMP SourceNumber,29
            JNZ Cont2
            StringToNumber P2Source
            MOV AX, 0
            Mov AL,P2Source+3    ;7, size, [, 1, ]
            MOV DI, offset P1Memory
            ADD DI, AX
            MOV BX, [DI]
            JMP Fcommand2

            Cont2:
            CMP SourceNumber,16
            JB DefaultSourcey
            ;IN AL
            Mov DI,Offset P1Registers
            Sub SourceNumber,16
            ADD DI,SourceNumber
            Mov Bh,0
            MOV Bl,[DI]
            JMP Fcommand2

            DefaultSourcey:
            Mov DI,Offset P1Registers
            ADD DI,SourceNumber
            MOV BX,[DI]

            
            Fcommand2:
            Call FindCommand 

            CMP DestinationNumber, 29
            JNZ DefaultDest6
            Mov SI,Offset P1Memory
            MOV CX,0
            Mov CL,P2Destination+3 
            ADD SI, CX
            MOV [SI],AX
            Call GMainScreen

            DefaultDest6:   ;Either AX or AL
            CMP DestinationNumber,16
            JB Dest2ByteR2
            ;IF 1Byte register
            Mov SI,Offset P1Registers
            Sub DestinationNumber,16
            ADD SI,DestinationNumber
            Mov [SI],AL
            Call GMainScreen

            Dest2ByteR2:
            Mov SI,Offset P1Registers
            ADD SI,DestinationNumber
            MOV [SI],AX
            Call GMainScreen

            ValueY:
            PushALL
            Call NumberIntoBX
            PopALL
            Mov BX,NumberHolder
            Call FindCommand 
            Mov SI,Offset P1Registers
            ADD SI,DestinationNumber
            MOV [SI],AX
            Call GMainScreen

            NoValueY:
            Mov Bx,0
            Call FindCommand 
            Mov SI,Offset P1Registers
            ADD SI,DestinationNumber
            MOV [SI],AX

            ENDHERE:
            Call GMainScreen
GMainScreen ENDP 

;5]] Show results
ShowScore Proc
    ;Change to video mode
    mov ah,0
    mov bh,0 
    mov al,13h
    int 10h

    mov ax, 1003h
    mov bx, 0
    int 10h

    
    ;Names
    MoveCursor 15,6
    DisplayStringRead P2Name
    
    ;Printing the screen
    CMP Winner,1
    JZ P2_Winner
    JMP P1_Winner
    
    P2_Winner:
    PushALL
    DrawDiagonalRLinePixel 140,180,60,33H
    DrawDiagonalPLinePixel 100,140,100,33H
    DrawDiagonalRLinePixel 140,180,70,34H
    DrawDiagonalPLinePixel 100,140,110,34H
    DrawDiagonalRLinePixel 140,180,80,35H
    DrawDiagonalPLinePixel 100,140,120,35H
    DrawDiagonalRLinePixel 140,170,90,35H
    DrawDiagonalPLinePixel 110,140,120,35H
    Mov SI,Offset PWinner
    DisplayCharacterSTR 040H,15,13,6

    MoveCursor 5,18
    DisplayString Scores
    MoveCursor 5,20
    HexaToDecimal P2_Score,P2_ScoreP
    DisplayString P2_ScoreP
    PopAll
    Mov Winner,0
    Call Exit

    P1_Winner:
    MoveCursor 5,10
    DisplayString Scores
    MoveCursor 5,12
    HexaToDecimal P2_Score,P2_ScoreP
    DisplayString P2_ScoreP
    Mov Winner,0
    Call Exit

ShowScore ENDP

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;           REGISTER             ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
FindRegisterDestination Proc
    CMP Flag,1
    JZ P1REGLBL
    JMP P2REGLBL

    P1REGLBL:
        Check1_AX:
        STRINGCOMPARE RAX,P1Destination,3,CommandNumber
        CMP CommandNumber,1
        JNZ Check1_BX
        Mov DestinationNumber,0
        RET

        Check1_BX:
        STRINGCOMPARE RBX,P1Destination,3,CommandNumber
        CMP CommandNumber,1
        JNZ Check1_CX
        Mov DestinationNumber,2
        RET

        Check1_CX:
        STRINGCOMPARE RCX,P1Destination,3,CommandNumber
        CMP CommandNumber,1
        JNZ Check1_DX
        Mov DestinationNumber,4
        RET

        Check1_DX:
        STRINGCOMPARE RDX,P1Destination,3,CommandNumber
        CMP CommandNumber,1
        JNZ Check1_SI
        Mov DestinationNumber,6
        RET

        Check1_SI:
        STRINGCOMPARE RSI,P1Destination,3,CommandNumber
        CMP CommandNumber,1
        JNZ Check1_DI
        Mov DestinationNumber,8
        RET

        Check1_DI:
        STRINGCOMPARE RDI,P1Destination,3,CommandNumber
        CMP CommandNumber,1
        JNZ Check1_SP
        Mov DestinationNumber,0AH
        RET

        Check1_SP:
        STRINGCOMPARE RSP,P1Destination,3,CommandNumber
        CMP CommandNumber,1
        JNZ Check1_BP
        Mov DestinationNumber,0CH
        RET

        Check1_BP:
        STRINGCOMPARE RBP,P1Destination,3,CommandNumber
        CMP CommandNumber,1
        JNZ Check1_AH
        Mov DestinationNumber,0EH
        RET
        

        ;Mai

        Check1_AH:
        Mov DL,P1Destination+2
        CMP DL,'A'
        JZ Achecked1
        JMP Check1_AL
        AChecked1:
        Mov DL,P1Destination+3
        CMP DL,'H'
        JZ IsAH1
        JMP Check1_Al
        IsAH1: 
        Mov DestinationNumber,017
        RET
        
        Check1_AL:
        STRINGCOMPARE RAL,P1Destination,2,CommandNumber
        CMP CommandNumber,1
        JNZ Check1_BH
        Mov DestinationNumber,016
        RET

        
        Check1_BH:
        Mov DL,P1Destination+2
        CMP DL,'B'
        JZ Bchecked1
        JMP Check1_BL
        BChecked1:
        Mov DL,P1Destination+3
        CMP DL,'H'
        JZ IsBH1
        JMP Check1_Bl
        IsBH1: 
        Mov DestinationNumber,019
        RET

        Check1_BL:
        STRINGCOMPARE RBL,P1Destination,2,CommandNumber
        CMP CommandNumber,1
        JNZ Check1_CH
        Mov DestinationNumber,018
        RET

 
        Check1_CH:
        Mov DL,P1Destination+2
        CMP DL,'C'
        JZ Cchecked1
        JMP Check1_CL
        CChecked1:
        Mov DL,P1Destination+3
        CMP DL,'H'
        JZ IsCH1
        JMP Check1_Cl
        IsCH1: 
        Mov DestinationNumber,21
        RET


        Check1_CL:
        STRINGCOMPARE RCLL,P1Destination,3,CommandNumber
        CMP CommandNumber,1
        JNZ Check1_DH
        Mov DestinationNumber,20
        RET

        
        Check1_DH:
        Mov DL,P1Destination+2
        CMP DL,'D'
        JZ Dchecked1
        JMP Check1_DL
        DChecked1:
        Mov DL,P1Destination+3
        CMP DL,'H'
        JZ IsDH1
        JMP Check1_Dl
        IsDH1: 
        Mov DestinationNumber,23
        RET



        Check1_DL:
        STRINGCOMPARE RDL,P1Destination,3,CommandNumber
        CMP CommandNumber,1
        JNZ Check1_Bracket
        Mov DestinationNumber,22
        RET
        

        Check1_Bracket:
        STRINGCOMPARE SBracket,P1Destination,2,CommandNumber
        CMP CommandNumber,1
        JNZ Check1_TYPO
        MOV DestinationNumber,29
        RET
        
        
        Check1_TYPO:  
        Mov BX,P1_Score
        CMP BX,1
        JAE P11Sufficient     ;Check if points are suitable
        RET
        P11Sufficient:
        DEC P1_Score
        RET
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


    P2REGLBL:
        Check2_AX:
        STRINGCOMPARE RAX,P2Destination,3,CommandNumber
        CMP CommandNumber,1
        JNZ Check2_BX
        Mov DestinationNumber,0
        RET

        Check2_BX:
        STRINGCOMPARE RBX,P2Destination,3,CommandNumber
        CMP CommandNumber,1
        JNZ Check2_CX
        Mov DestinationNumber,2
        RET

        Check2_CX:
        STRINGCOMPARE RCX,P2Destination,3,CommandNumber
        CMP CommandNumber,1
        JNZ Check2_DX
        Mov DestinationNumber,4
        RET

        Check2_DX:
        STRINGCOMPARE RDX,P2Destination,3,CommandNumber
        CMP CommandNumber,1
        JNZ Check2_SI
        Mov DestinationNumber,6
        RET

        Check2_SI:
        STRINGCOMPARE RSI,P2Destination,3,CommandNumber
        CMP CommandNumber,1
        JNZ Check2_DI
        Mov DestinationNumber,8
        RET

        Check2_DI:
        STRINGCOMPARE RDI,P2Destination,3,CommandNumber
        CMP CommandNumber,1
        JNZ Check2_SP
        Mov DestinationNumber,0AH
        RET

        Check2_SP:
        STRINGCOMPARE RSP,P2Destination,3,CommandNumber
        CMP CommandNumber,1
        JNZ Check2_BP
        Mov DestinationNumber,0CH
        RET

        Check2_BP:
        STRINGCOMPARE RBP,P2Destination,3,CommandNumber
        CMP CommandNumber,1
        JNZ Check2_AH
        Mov DestinationNumber,0EH
        RET

        Check2_AH:
        Mov DL,P2Destination+3
        CMP DL,'A'
        JZ Achecked2
        JMP Check2_AL
        AChecked2:
        Mov DL,P2Destination+4
        CMP DL,'H'
        JZ IsAH2
        JMP Check2_Al
        IsAH2: 
        Mov DestinationNumber,017
        RET

        Check2_AL:
        STRINGCOMPARE RAL,P2Destination,3,CommandNumber
        CMP CommandNumber,1
        JNZ Check2_BH
        Mov DestinationNumber,016
        RET

        Check2_BH:
        Mov DL,P2Destination+3
        CMP DL,'B'
        JZ Bchecked2
        JMP Check2_BL
        BChecked2:
        Mov DL,P2Destination+4
        CMP DL,'H'
        JZ IsBH2
        JMP Check2_Bl
        IsBH2: 
        Mov DestinationNumber,019
        RET

        Check2_BL:
        STRINGCOMPARE RBL,P2Destination,3,CommandNumber
        CMP CommandNumber,1
        JNZ Check2_CH
        Mov DestinationNumber,018
        RET


        Check2_CH:
        Mov DL,P2Destination+3
        CMP DL,'C'
        JZ Cchecked2
        JMP Check2_CL
        CChecked2:
        Mov DL,P2Destination+4
        CMP DL,'H'
        JZ IsCH2
        JMP Check2_Cl
        IsCH2: 
        Mov DestinationNumber,21
        RET


        Check2_CL:
        STRINGCOMPARE RCLL,P2Destination,3,CommandNumber
        CMP CommandNumber,1
        JNZ Check2_DH
        Mov DestinationNumber,20
        RET

        Check2_DH:
        Mov DL,P2Destination+3
        CMP DL,'D'
        JZ Dchecked2
        JMP Check2_DL
        DChecked2:
        Mov DL,P2Destination+4
        CMP DL,'H'
        JZ IsDH2
        JMP Check2_Dl
        IsDH2: 
        Mov DestinationNumber,023
        RET
        
        Check2_DL:
        STRINGCOMPARE RDL,P2Destination,3,CommandNumber
        CMP CommandNumber,1
        JNZ Check2_Bracket
        Mov DestinationNumber,22
        RET

        Check2_Bracket:
        STRINGCOMPARE SBracket,P2Destination,2,CommandNumber
        CMP CommandNumber,1
        JNZ Check22_TYPO
        MOV DestinationNumber,29
        RET

        Check22_TYPO:  
        Mov BX,P2_Score
        CMP BX,1
        JAE P22Sufficient     ;Check if points are suitable
        RET
        P22Sufficient:
        DEC P2_Score
        RET

FindRegisterDestination ENDP

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
FindRegisterSource Proc
   CMP Flag,1
    JZ P1REGLBL1
    JMP P2REGLBL1

    P1REGLBL1:
        Check3_AX:
        STRINGCOMPARE RAX,P1Source,3,CommandNumber
        CMP CommandNumber,1
        JNZ Check3_BX
        Mov SourceNumber,0
        RET

        Check3_BX:
        STRINGCOMPARE RBX,P1Source,3,CommandNumber
        CMP CommandNumber,1
        JNZ Check3_CX
        Mov SourceNumber,2
        RET

        Check3_CX:
        STRINGCOMPARE RCX,P1Source,3,CommandNumber
        CMP CommandNumber,1
        JNZ Check3_DX
        Mov SourceNumber,4
        RET

        Check3_DX:
        STRINGCOMPARE RDX,P1Source,3,CommandNumber
        CMP CommandNumber,1
        JNZ Check3_SI
        Mov SourceNumber,6
        RET

        Check3_SI:
        STRINGCOMPARE RSI,P1Source,3,CommandNumber
        CMP CommandNumber,1
        JNZ Check3_DI
        Mov SourceNumber,8
        RET

        Check3_DI:
        STRINGCOMPARE RDI,P1Source,3,CommandNumber
        CMP CommandNumber,1
        JNZ Check3_SP
        Mov SourceNumber,0AH
        RET

        Check3_SP:
        STRINGCOMPARE RSP,P1Source,3,CommandNumber
        CMP CommandNumber,1
        JNZ Check3_BP
        Mov SourceNumber,0CH
        RET

        Check3_BP:
        STRINGCOMPARE RBP,P1Source,3,CommandNumber
        CMP CommandNumber,1
        JNZ Check3_AH
        Mov SourceNumber,0EH
        RET


        Check3_AH:
        Mov DL,P1Source+2
        CMP DL,'A'
        JZ Achecked3
        JMP Check3_AL
        AChecked3:
        Mov DL,P1Source+3
        CMP DL,'H'
        JZ IsAH3
        JMP Check3_Al
        IsAH3: 
        Mov SourceNumber,17
        RET


        Check3_AL:
        STRINGCOMPARE RAL,P1Source,3,CommandNumber
        CMP CommandNumber,1
        JNZ Check3_BH
        Mov SourceNumber,16
        RET

        
        Check3_BH:
        Mov DL,P1Source+2
        CMP DL,'B'
        JZ Bchecked3
        JMP Check3_BL
        BChecked3:
        Mov DL,P1Source+3
        CMP DL,'H'
        JZ IsBH3
        JMP Check3_Bl
        IsBH3: 
        Mov SourceNumber,19
        RET


        Check3_BL:
        STRINGCOMPARE RBL,P1Source,3,CommandNumber
        CMP CommandNumber,1
        JNZ Check3_CH
        Mov SourceNumber,18
        RET

        Check3_CH:
        Mov DL,P1Source+2
        CMP DL,'C'
        JZ Cchecked3
        JMP Check3_CL
        CChecked3:
        Mov DL,P1Source+3
        CMP DL,'H'
        JZ IsCH3
        JMP Check3_Cl
        IsCH3: 
        Mov SourceNumber,21
        RET

        Check3_CL:
        STRINGCOMPARE RCLL,P1Source,3,CommandNumber
        CMP CommandNumber,1
        JNZ Check3_DH
        Mov SourceNumber,20
        RET

        Check3_DH:
        Mov DL,P1Source+2
        CMP DL,'D'
        JZ Dchecked3
        JMP Check3_DL
        DChecked3:
        Mov DL,P1Source+3
        CMP DL,'H'
        JZ IsDH3
        JMP Check3_Dl
        IsDH3: 
        Mov SourceNumber,23
        RET

        Check3_DL:
        STRINGCOMPARE RDL,P1Source,3,CommandNumber
        CMP CommandNumber,1
        JNZ Check3_Bracket
        Mov SourceNumber,22
        RET

        Check3_Bracket:
        STRINGCOMPARE SBracket,P1Source,2,CommandNumber
        CMP CommandNumber,1
        JNZ Check3_Number
        MOV SourceNumber,29
        RET

        Check3_Number:
        Mov SI,Offset P1Source + 1 
        Mov Dl,[SI]
        CMP Dl,0
        JZ NOSource
        MOV SourceNumber,30
        StringToNumber P1Source
        RET

        NOSource:   
        Mov SourceNumber,31
        RET

    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


    P2REGLBL1:
        Check4_AX:
        STRINGCOMPARE RAX,P2Source,3,CommandNumber
        CMP CommandNumber,1
        JNZ Check4_BX
        Mov SourceNumber,0
        RET

        Check4_BX:
        STRINGCOMPARE RBX,P2Source,3,CommandNumber
        CMP CommandNumber,1
        JNZ Check4_CX
        Mov SourceNumber,2
        RET

        Check4_CX:
        STRINGCOMPARE RCX,P2Source,3,CommandNumber
        CMP CommandNumber,1
        JNZ Check4_DX
        Mov SourceNumber,4
        RET

        Check4_DX:
        STRINGCOMPARE RDX,P2Source,3,CommandNumber
        CMP CommandNumber,1
        JNZ Check4_SI
        Mov SourceNumber,6
        RET

        Check4_SI:
        STRINGCOMPARE RSI,P2Source,3,CommandNumber
        CMP CommandNumber,1
        JNZ Check4_DI
        Mov SourceNumber,8
        RET

        Check4_DI:
        STRINGCOMPARE RDI,P2Source,3,CommandNumber
        CMP CommandNumber,1
        JNZ Check4_SP
        Mov SourceNumber,0AH
        RET

        Check4_SP:
        STRINGCOMPARE RSP,P2Source,3,CommandNumber
        CMP CommandNumber,1
        JNZ Check4_BP
        Mov SourceNumber,0CH
        RET

        Check4_BP:
        STRINGCOMPARE RBP,P2Source,3,CommandNumber
        CMP CommandNumber,1
        JNZ Check4_AH
        Mov SourceNumber,0EH
        RET

        Check4_AH:
        Mov DL,P2Source+2
        CMP DL,'A'
        JZ Achecked4
        JMP Check4_AL
        AChecked4:
        Mov DL,P2Source+3
        CMP DL,'H'
        JZ IsAH4
        JMP Check4_Al
        IsAH4: 
        Mov SourceNumber,17
        RET


        Check4_AL:
        STRINGCOMPARE RAL,P2Source,3,CommandNumber
        CMP CommandNumber,1
        JNZ Check4_BH
        Mov SourceNumber,16
        RET

        Check4_BH:
        Mov DL,P2Source+2
        CMP DL,'B'
        JZ Bchecked4
        JMP Check4_BL
        BChecked4:
        Mov DL,P2Source+3
        CMP DL,'H'
        JZ IsAH4
        JMP Check4_Bl
        IsBH4: 
        Mov SourceNumber,19
        RET

        Check4_BL:
        STRINGCOMPARE RBL,P2Source,3,CommandNumber
        CMP CommandNumber,1
        JNZ Check4_CH
        Mov SourceNumber,18
        RET

        Check4_CH:
        Mov DL,P2Source+2
        CMP DL,'C'
        JZ Cchecked4
        JMP Check4_CL
        CChecked4:
        Mov DL,P2Source+3
        CMP DL,'H'
        JZ IsCH4
        JMP Check4_Cl
        IsCH4: 
        Mov SourceNumber,21
        RET

        Check4_CL:
        STRINGCOMPARE RCLL,P2Source,3,CommandNumber
        CMP CommandNumber,1
        JNZ Check4_DH
        Mov SourceNumber,20
        RET

        Check4_DH:
        Mov DL,P2Source+2
        CMP DL,'D'
        JZ Dchecked4
        JMP Check4_DL
        DChecked4:
        Mov DL,P2Source+3
        CMP DL,'H'
        JZ IsDH4
        JMP Check4_Dl
        IsDH4: 
        Mov SourceNumber,23
        RET

        Check4_DL:
        STRINGCOMPARE RDL,P2Source,3,CommandNumber
        CMP CommandNumber,1
        JNZ Check4_Bracket
        Mov SourceNumber,22
        RET

        Check4_Bracket:
        STRINGCOMPARE SBracket,P2Source,2,CommandNumber
        CMP CommandNumber,1
        JNZ Check4_Number
        MOV SourceNumber,29
        RET

        Check4_Number:
        Mov SI,Offset P2Source + 1 
        Mov Dl,[SI]
        CMP Dl,0
        JZ  NOSource1
        MOV SourceNumber,30
        StringToNumber P2Source
        RET

        NOSource1:   
        Mov SourceNumber,31
        RET

FindRegisterSource ENDP

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;                                ;
;           Command              ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
FindCommand Proc
    ;;PLAAAAYER (  1  ) COMMAAND;;
    CMP Flag,1
    JZ P1COMLBL
    JMP P2COMLBL

    P1COMLBL:
    Check1_MOV:
    STRINGCOMPARE ComMOV,P1Command,4,CommandNumber
    CMP CommandNumber,1
    JNZ Check1_SUB
    Mov AX,BX
    RET

    Check1_SUB:
    STRINGCOMPARE ComSUB,P1Command,4,CommandNumber
    CMP CommandNumber,1
    JNZ Check1_Add
    SUB AX,BX
   
    RET

    Check1_ADD:
    STRINGCOMPARE ComADD,P1Command,4,CommandNumber
    CMP CommandNumber,1
    JNZ Check1_ADC
    ADD AX,BX
   
    RET

    Check1_ADC:
    STRINGCOMPARE ComADC,P1Command,4,CommandNumber
    CMP CommandNumber,1
    JNZ Check1_SBB
    ADC AX,BX
   
    RET

    Check1_SBB:
    STRINGCOMPARE ComSBB,P1Command,4,CommandNumber
    CMP CommandNumber,1
    JNZ Check1_DIV
    SBB AX,BX
   
    RET

    Check1_DIV:
    STRINGCOMPARE ComDIV,P1Command,4,CommandNumber
    CMP CommandNumber,1
    JNZ Check1_MUL
    DIV BX
   
    RET

    Check1_MUL:
    STRINGCOMPARE ComMUL,P1Command,4,CommandNumber
    CMP CommandNumber,1
    JNZ Check1_XOR
    MUL BX
   
    RET

    Check1_XOR:
    STRINGCOMPARE ComXOR,P1Command,4,CommandNumber
    CMP CommandNumber,1
    JNZ Check1_AND
    XOR AX,BX
   
    RET

    Check1_AND:
    STRINGCOMPARE ComAND,P1Command,4,CommandNumber
    CMP CommandNumber,1
    JNZ Check1_NOP
    AND AX,BX
   
    RET

    Check1_NOP:
    STRINGCOMPARE ComNOP,P1Command,4,CommandNumber
    CMP CommandNumber,1
    JNZ Check1_SHR
    NOP
   
    RET

    Check1_SHR:
    STRINGCOMPARE ComSHR,P1Command,4,CommandNumber
    CMP CommandNumber,1
    JNZ Check1_SHL
    Mov CH,0
    Mov Cl,Bl
    SHR AX,Cl
   
    RET

    Check1_SHL:
    STRINGCOMPARE ComSHL,P1Command,4,CommandNumber
    CMP CommandNumber,1
    JNZ Check1_SAR
    Mov CH,0
    Mov Cl,Bl
    SHL AX,CL
   
    RET

    Check1_SAR:
    STRINGCOMPARE ComSAR,P1Command,4,CommandNumber
    CMP CommandNumber,1
    JNZ Check1_CLC
    Mov CH,0
    Mov Cl,Bl
    SAR AX,Cl
   
    RET

    Check1_CLC:
    STRINGCOMPARE ComCLC,P1Command,4,CommandNumber
    CMP CommandNumber,1
    JNZ Check1_ROR
    CLC
   
    RET

    Check1_ROR:
    STRINGCOMPARE ComROR,P1Command,4,CommandNumber
    CMP CommandNumber,1
    JNZ Check1_RCL
    Mov CH,0
    Mov Cl,Bl
    ROR AX,Cl
   
    RET

    Check1_RCL:
    STRINGCOMPARE ComRCL,P1Command,4,CommandNumber
    CMP CommandNumber,1
    JNZ Check1_RCR
    Mov CH,0
    Mov Cl,Bl
    RCL AX,Cl
   
    RET

    Check1_RCR:
    STRINGCOMPARE ComRCR,P1Command,4,CommandNumber
    CMP CommandNumber,1
    JNZ Check1_ROL
    Mov CH,0
    Mov Cl,Bl
    RCR AX,Cl
   
    RET

    Check1_ROL:
    STRINGCOMPARE ComROL,P1Command,4,CommandNumber
    CMP CommandNumber,1
    JNZ Check1_INC
    Mov CH,0
    Mov Cl,Bl
    ROL AX,Cl
   
    RET

    Check1_INC:
    STRINGCOMPARE ComINC,P1Command,4,CommandNumber
    CMP CommandNumber,1
    JNZ Check1_DEC
    INC AX
   
    RET

    Check1_DEC:
    STRINGCOMPARE ComDEC,P1Command,4,CommandNumber
    CMP CommandNumber,1
    JNZ Check1_OR
    DEC AX
   
    RET

    Check1_OR:
    STRINGCOMPARE ComOR,P1Command,2,CommandNumber
    CMP CommandNumber,1
    JNZ Check1_Push
    OR AX,BX
    RET

    Check1_Push:
    STRINGCOMPARE ComPush,P1Command,4,CommandNumber
    CMP CommandNumber,1
    JNZ Check_TYPO
    CMP DestinationNumber,16
    JA NOPUSH
    Mov SI,Offset P2Memory+13
    Mov [SI],Ax
    NOPUSH:
    Dec P1_Score
    Call GMainScreen

    
    Check_TYPO:
    Mov BX,P1_Score
    CMP BX,1
    JAE C11Sufficient     ;Check if points are suitable
    RET
    C11Sufficient:
    DEC P1_Score
    RET
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

    P2COMLBL:
    Check2_MOV:
    STRINGCOMPARE ComMOV,P2Command,4,CommandNumber
    CMP CommandNumber,1
    JNZ Check2_SUB
    Mov AX,BX
    RET

    Check2_SUB:
    STRINGCOMPARE ComSUB,P2Command,4,CommandNumber
    CMP CommandNumber,1
    JNZ Check2_Add
    SUB AX,BX
    RET

    Check2_ADD:
    STRINGCOMPARE ComADD,P2Command,4,CommandNumber
    CMP CommandNumber,1
    JNZ Check2_ADC
    ADD AX,BX
    RET

    Check2_ADC:
    STRINGCOMPARE ComADC,P2Command,4,CommandNumber
    CMP CommandNumber,1
    JNZ Check2_SBB
    ADC AX,BX
    RET

    Check2_SBB:
    STRINGCOMPARE ComSBB,P2Command,4,CommandNumber
    CMP CommandNumber,1
    JNZ Check2_DIV
    SBB AX,BX
    RET

    Check2_DIV:
    STRINGCOMPARE ComDIV,P2Command,4,CommandNumber
    CMP CommandNumber,1
    JNZ Check2_MUL
    DIV BX
    RET

    Check2_MUL:
    STRINGCOMPARE ComMUL,P2Command,4,CommandNumber
    CMP CommandNumber,1
    JNZ Check2_XOR
    MUL BX
    RET

    Check2_XOR:
    STRINGCOMPARE ComXOR,P2Command,4,CommandNumber
    CMP CommandNumber,1
    JNZ Check2_AND
    XOR AX,BX
    RET

    Check2_AND:
    STRINGCOMPARE ComAND,P2Command,4,CommandNumber
    CMP CommandNumber,1
    JNZ Check2_NOP
    AND AX,BX
    RET

    Check2_NOP:
    STRINGCOMPARE ComNOP,P2Command,4,CommandNumber
    CMP CommandNumber,1
    JNZ Check2_SHR
    NOP
    RET

    Check2_SHR:
    STRINGCOMPARE ComSHR,P2Command,4,CommandNumber
    CMP CommandNumber,1
    JNZ Check2_SHL
    Mov CH,0
    Mov Cl,Bl
    SHR AX,Cl
    RET

    Check2_SHL:
    STRINGCOMPARE ComSHL,P2Command,4,CommandNumber
    CMP CommandNumber,1
    JNZ Check2_SAR
    Mov CH,0
    Mov Cl,Bl
    SHL AX,Cl
    RET

    Check2_SAR:
    STRINGCOMPARE ComSAR,P2Command,4,CommandNumber
    CMP CommandNumber,1
    JNZ Check2_CLC
    Mov CH,0
    Mov Cl,Bl
    SAR AX,Cl
    RET

    Check2_CLC:
    STRINGCOMPARE ComCLC,P2Command,4,CommandNumber
    CMP CommandNumber,1
    JNZ Check2_ROR
    CLC
    RET

    Check2_ROR:
    STRINGCOMPARE ComROR,P2Command,4,CommandNumber
    CMP CommandNumber,1
    JNZ Check2_RCL
    Mov CH,0
    Mov Cl,Bl
    ROR AX,Cl
    RET

    Check2_RCL:
    STRINGCOMPARE ComRCL,P2Command,4,CommandNumber
    CMP CommandNumber,1
    JNZ Check2_RCR
    Mov CH,0
    Mov Cl,Bl
    RCL AX,Cl
    RET

    Check2_RCR:
    STRINGCOMPARE ComRCR,P2Command,4,CommandNumber
    CMP CommandNumber,1
    JNZ Check2_ROL
    Mov CH,0
    Mov Cl,Bl
    RCR AX,Cl
    RET

    Check2_ROL:
    STRINGCOMPARE ComROL,P2Command,4,CommandNumber
    CMP CommandNumber,1
    JNZ Check2_INC
    Mov CH,0
    Mov Cl,Bl
    ROL AX,Cl
    RET

    Check2_INC:
    STRINGCOMPARE ComINC,P2Command,4,CommandNumber
    CMP CommandNumber,1
    JNZ Check2_DEC
    INC AX
    RET

    Check2_DEC:
    STRINGCOMPARE ComDEC,P2Command,4,CommandNumber
    CMP CommandNumber,1
    JNZ Check2_OR
    DEC AX
    RET

    Check2_OR:
    STRINGCOMPARE ComOR,P2Command,2,CommandNumber
    CMP CommandNumber,1
    JNZ Check2_Push
    OR AX,BX
    RET

    Check2_Push:
    STRINGCOMPARE ComPush,P2Command,4,CommandNumber
    CMP CommandNumber,1
    JNZ Check_TYPOS
    CMP DestinationNumber,16
    JA NOPUSH1
    Mov SI, Offset P1Memory+13
    Mov [SI],Ax
    NOPUSH1:
    Dec P2_Score
    Call GMainScreen

    Check_TYPOS:
    Mov BX,P2_Score
    CMP BX,1
    JAE C22Sufficient     ;Check if points are suitable
    RET
    C22Sufficient:
    DEC P2_Score
    RET
FindCommand ENDP

;..................................
;           Chat Screen           ;
;..................................   
ChatMode Proc 
    Clear
    Call InitializationSerial
    ;Designing the screen 
    ;Drawing a vertical line at the middle if the Screen 
    Mov Cl,25D
    Mov Bl,0
    VrLine: 
    MoveCursor 40,Bl
        Mov Ah,2
        Mov Dl, '|'
        int 21H
        INC Bl
        DEC Cl
        JNZ VrLine
        
    MoveCursor 0,0
    DisplayStringRead P1Name
    MoveCursor 41,0
    DisplayStringRead P2Name

    ;BL will contain the Y-Coordinates of messeges in chat 
    Mov BL,1

    ;Player1 1)Read then 2)Print on screen
    ChatP1:
    call recChar
    Mov Ah,Val
    CMP AH,3Dh
    JNZ NoExitChat
    JMP far ptr SecondS
    NoExitChat: 
    
    Mov Cx,50
    call recieve
    STRINGCOPY Chat_2,P1Chat,50
    MoveCursor 2,BL
    DisplayStringRead P1Chat
    PushALL
    ;TO Clear String 
    Mov SI,Offset P1Chat+2
    Mov CX,50
    ZLOOP:
    Mov Al,SDollar
    Mov [SI],Al
    INC SI
    DEC CX
    JNZ ZLOOP
        ;Clear the TextBox
        mov ax,0600h 
        mov bh,07 
        mov cx,1800H 
        mov dx,1927H
        int 10h    
    PopALL
    INC BL
    JMP ChatP2

    ReturntoMain: Call SecondS

    ;Player2 1)Read then 2)Print on screen
    ChatP2: 
    ;To Chech that F3 Is Not pressed 
        mov ah,0
        int 16h
    Mov Val,Ah
    call sendChar
    CMP AH,3Dh
    JNZ NoExitChat1 
    Jmp far ptr SecondS
    NoExitChat1:  
    MoveCursor 41,24 ;Place to read from
    ReadString P2Chat
    MoveCursor 43,BL
    DisplayStringRead P2Chat
    STRINGCOPY P2Chat,Chat_2,50
    Mov Cx,50
    Call send

    PushALL
    ;TO Clear String 
    Mov SI,Offset P2Chat+2
    Mov CX,50
    MLOOP:
    Mov Al,SDollar
    Mov [SI],Al
    INC SI
    DEC CX
    JNZ MLOOP
    ;Clear the TextBox
        mov ax,0600h 
        mov bh,07 
        mov cx,1829h 
        mov dx,1950h
        int 10h
    PopALL
    INC BL

    ;To Scroll screen after chat
    CMP Bl,16H
    JNC ScrollChat
    JMP ChatP1


    ScrollChat:
    PushALL
        mov ax,060Ah 
        mov bh,07 
        mov cx,0100H 
        mov dx,1927H
        int 10H
        
        mov ax,060Ah 
        mov bh,07 
        mov cx,0129h 
        mov dx,1950h
        int 10h

    PopALL

        Sub Bl,10
        JMP ChatP1
ChatMode ENDP

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;                         FUNCTION  COMMAND                                 ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
MakeALLCapital Proc
    PushALL
    MakeCapital P1Command,4
    MakeCapital P2Command,4

    MakeCapital P1Source,2
    MakeCapital P2Source,2

    MakeCapital P1Destination,2
    MakeCapital P2Destination,2
    PopALL
    RET
MakeALLCapital ENDP

ClearALLRegisters Proc    ;Works on BL => 1)ClearAll, 2)ClearP1Registers, 3)ClearP2Registers
    CMP BL,1
    JNZ Clear1
    Mov SI,Offset P1Registers
    Mov Ax,0
    Mov CX,16
    JMP LOOPC

    Clear1:
    CMP Bl,2
    JNZ Clear2
    Mov SI,Offset P1Registers
    Mov Ax,0
    Mov CX,8
    JMP LOOPC

    Clear2:
    CMP Bl,3
    JNZ LOOPC
    Mov SI,Offset P2Registers
    Mov Ax,0
    Mov CX,8

    LOOPC: 
    Mov [SI],AX
    ADD SI,2
    DEC CX
    JNZ LOOPC
    RET
ClearALLRegisters ENDP

DecNumberIntoBX Proc
    CMP Flag,1
    JZ NP1D
    JMP NP2D

    NP1D:
    Mov SI,offset P1Source + 1      
    Mov Cl,[SI]
    AD:
    CMP Cl,1
    JNZ BD
    Mov AX,0
    Mov SI,Offset P1Source + 2
    Mov Al,[SI]
    Mov NumberHolder,AX
    RET
    BD:
    CMP Cl,2     ;1 2   ;AX=0012   AX=0102
    JNZ CD
    Mov AX,0
    Mov SI,Offset P1Source + 2
    Mov Al,[SI]
    INC SI
    Mov Bl,[SI]
    Mov Ch,10
    MUL Ch
    ADD Al,Bl
    Mov NumberHolder,AX
    RET
    CD:
    CMP Cl,3
    JNZ DD1        ;1  2  3
    Mov AX,0
    Mov SI,Offset P1Source + 2
    Mov Al,[SI]
    INC SI
    Mov Bl,[SI]
    Mov Ch,100
    MUL Ch
    Mov DX,AX
    Mov AX,0
    Mov SI,Offset P1Source + 3
    Mov Al,[SI]
    INC SI
    Mov Bl,[SI]
    Mov Ch,10
    MUL Ch
    ADD Al,Bl
    ADD DX,AX
    Mov NumberHolder,DX
    RET
    DD1:
    CMP Cl,4   ;1 2 3 4
    JNZ LOOPENDD
    Mov AX,0
    Mov SI,Offset P1Source + 2
    Mov Al,[SI]
    INC SI
    Mov Bl,[SI]
    Mov Ch,10
    MUL Ch
    ADD Al,Bl
    Mov Dh,AL
    Mov Dl,0  ;DX=1200

    Mov AX,0
    Mov SI,Offset P1Source + 4
    Mov Al,[SI]
    INC SI
    Mov Bl,[SI]
    Mov Ch,10
    MUL Ch
    ADD Al,Bl
    Mov Dl,Al
    Mov NumberHolder,DX

    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

    LOOPENDD:RET

    NP2D:
    Mov SI,offset P2Source + 1      
    Mov Cl,[SI]
    A1D:
    CMP Cl,1
    JNZ B1D
    Mov AX,0
    Mov SI,Offset P2Source + 2
    Mov Al,[SI]
    Mov NumberHolder,AX
    RET
    B1D:
    CMP Cl,2     ;1 2   ;AX=0012   AX=0102
    JNZ C1D
    Mov AX,0
    Mov SI,Offset P2Source + 2
    Mov Al,[SI]
    INC SI
    Mov Bl,[SI]
    Mov Ch,10
    MUL Ch
    ADD Al,Bl
    Mov NumberHolder,AX
    RET
    C1D:
    CMP Cl,3
    JNZ D1D        ;1  2  3
    Mov AX,0
    Mov SI,Offset P2Source + 2
    Mov Al,[SI]
    INC SI
    Mov Bl,[SI]
    Mov Ch,100
    MUL Ch
    Mov DX,AX
    Mov AX,0
    Mov SI,Offset P2Source + 3
    Mov Al,[SI]
    INC SI
    Mov Bl,[SI]
    Mov Ch,10
    MUL Ch
    ADD Al,Bl
    ADD DX,AX
    Mov NumberHolder,DX
    RET
    D1D:
    CMP Cl,4   ;1 2 3 4
    JNZ LOOPENDD
    Mov AX,0
    Mov SI,Offset P2Source + 2
    Mov Al,[SI]
    INC SI
    Mov Bl,[SI]
    Mov Ch,10
    MUL Ch
    ADD Al,Bl
    Mov Dh,AL
    Mov Dl,0  ;DX=1200

    Mov AX,0
    Mov SI,Offset P2Source + 4
    Mov Al,[SI]
    INC SI
    Mov Bl,[SI]
    Mov Ch,10
    MUL Ch
    ADD Al,Bl
    Mov Dl,Al
    Mov NumberHolder,DX 
DecNumberIntoBX ENDP

CheckTarget Proc
    PushALL
    PUSH SI 
    
    Mov SI, Offset P1Registers
    Mov BL,8
    Target1_loop:
    Mov Ax,[SI]
    CMP AX,Target
    JZ FoundinP1
    Add SI,2
    Dec BL
    JNZ Target1_loop
    JMP NotFoundinP1
    FoundinP1: Mov Winner,1
    
    NotFoundinP1:
    Mov BX, Offset P2Registers
    Mov CH,8
    Target2_loop:
    Mov Ax,[BX]
    CMP AX,Target
    JZ FoundinP2
    Add BX,2
    Dec CH
    JNZ Target2_loop
    JMP NotFoundinP2
    FoundinP2: Mov Winner,2
    NotFoundinP2:

    POP SI
    PopALL
    RET
CheckTarget ENDP

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
NumberIntoBX Proc
    CMP Flag,1
    JZ NP1
    JMP NP2

    NP1:
    Mov SI,offset P1Source + 1      
    Mov Cl,[SI]
    A:
    CMP Cl,1
    JNZ B
    Mov AX,0
    Mov SI,Offset P1Source + 2
    Mov Al,[SI]
    Mov NumberHolder,AX
    RET
    B:
    CMP Cl,2     ;1 2   ;AX=0012   AX=0102
    JNZ C
    Mov AX,0
    Mov SI,Offset P1Source + 2    ;Al=0000 0001    Bl=0000 0010
    Mov Al,[SI]
    INC SI
    Mov Bl,[SI]
    Push Cx
    Mov Cl,4
    SHL Al,Cl                      ;Al=0001 0000
    Pop Cx
    ADD Al,Bl                     ;Al=0001 0010
    Mov NumberHolder,AX
    RET
    C:
    CMP Cl,3
    JNZ D         
    Mov AX,0
    Mov SI,Offset P1Source + 2
    Mov Al,[SI]
    INC SI
    Mov Bl,[SI]
    Push CX
    Mov Cl,4
    SHL AX,Cl
    Pop Cx
    Add Al,Bl
    INC SI
    Mov Bl,[SI]
    Push CX
    Mov Cl,4
    SHL AX,Cl
    Pop Cx
    Add Al,Bl
    Mov NumberHolder,AX
    RET
    D:
    PushALL
    PUSH SI
    CMP Cl,4   ;1 2 3 4
    JNZ LOOPEND         
    Mov AX,0
    Mov SI,Offset P1Source + 2
    Mov Al,[SI]
    INC SI
    Mov Bl,[SI]
    Push CX
    Mov Cl,4
    SHL AX,Cl
    Pop Cx
    Add Al,Bl
    INC SI
    Mov Bl,[SI]
    Push CX
    Mov Cl,4
    SHL AX,Cl
    Pop Cx
    Add Al,Bl
    Inc SI
    Mov Bl,[SI]
    Push CX
    Mov Cl,4
    SHL AX,Cl
    Pop Cx
    Add Al,Bl
    Mov NumberHolder,AX
    POP SI 
    POPALL
    RET

    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

    LOOPEND:RET

    NP2:
    Mov SI,offset P2Source + 1      
    Mov Cl,[SI]
    A1:
    CMP Cl,1
    JNZ B1
    Mov AX,0
    Mov SI,Offset P2Source + 2
    Mov Al,[SI]
    Mov NumberHolder,AX
    RET
    B1:
    CMP Cl,2     ;1 2   ;AX=0012   AX=0102
    JNZ C1
    Mov AX,0                     
    Mov SI,Offset P2Source + 2     ;;0001 0010
    Mov Al,[SI]
    INC SI
    Mov Bl,[SI]
    Push CX
    Mov Cl,4
    SHL AX,Cl
    Pop Cx
    ADD Al,Bl
    Mov NumberHolder,AX
    RET
    C1:
    CMP Cl,3
    JNZ D1        
    Mov AX,0
    Mov SI,Offset P2Source + 2
    Mov Al,[SI]
    INC SI
    Mov Bl,[SI]
    Push CX
    Mov Cl,4
    SHL AX,Cl
    Pop Cx
    Add Al,Bl
    INC SI
    Mov Bl,[SI]
    Push CX
    Mov Cl,4
    SHL AX,Cl
    Pop Cx
    Add Al,Bl
    Mov NumberHolder,AX
    RET
    D1:
    PushALL
    PUSH SI
    CMP Cl,4 
    JNZ LOOPEND         
    Mov AX,0
    Mov SI,Offset P2Source + 2
    Mov Al,[SI]
    INC SI
    Mov Bl,[SI]
    Push CX
    Mov Cl,4
    SHL AX,Cl
    Pop Cx
    Add Al,Bl
    INC SI
    Mov Bl,[SI]
    Push CX
    Mov Cl,4
    SHL AX,Cl
    Pop Cx
    Add Al,Bl
    Inc SI
    Mov Bl,[SI]
    Push CX
    Mov Cl,4
    SHL AX,Cl
    Pop Cx
    Add Al,Bl
    Mov NumberHolder,AX
    POP SI 
    POPALL
    RET
NumberIntoBX ENDP

InitializationSerial Proc
    mov dx,3fbh 			; Line Control Register
    mov al,10000000b		;Set Divisor Latch Access Bit
    out dx,al				;Out it
    mov dx,3f8h			
    mov al,0ch			
    out dx,al

    mov dx,3f9h
    mov al,00h
    out dx,al

    mov dx,3fbh
    mov al,00011011b
    out dx,al

    ret
InitializationSerial ENDP 

send proc near
;Check that Transmitter Holding Register is Empty
        mov si,2
  		looping:
        mov dx , 3FDH		; Line Status Register
AGAIN:  	In al , dx 			;Read Line Status
  		test al , 00100000b
  		JZ AGAIN                               ;Not empty

;If empty put the VALUE in Transmit data register

  		mov dx , 3F8H ; Transmit data register
  		mov al,Chat_2[si]
  		inc si
        out dx , al 
        loop looping

  		
ret
send endp

recieve proc near
;Check that Data is Ready
        mov si,2 
  		looping2:
        mov dx , 3FDH		; Line Status Register
	CHK:	in al , dx 
  		test al , 1
  		JZ CHK                                    ;Not Ready
 ;If Ready read the VALUE in Receive data register
  		mov dx , 03F8H
        

  		
  		in al , dx
  		mov Chat_2[si],al
        inc si
        loop looping2
        ret
recieve endp

sendChar proc near
;Check that Transmitter Holding Register is Empty

        mov dx , 3FDH		; Line Status Register
AGAIN1:  	In al , dx 			;Read Line Status
  		test al , 00100000b
  		JZ AGAIN1                               ;Not empty

;If empty put the VALUE in Transmit data register
  		mov dx , 3F8H ; Transmit data register
  		mov al,Val
        out dx , al

  		
ret
sendChar endp

recChar proc near
;Check that Data is Ready

        mov dx , 3FDH		; Line Status Register
	CHKS:	in al , dx 
  		test al , 1
  		JZ CHKS                                    ;Not Ready
 ;If Ready read the VALUE in Receive data register
  		mov dx , 03F8H
        
          
  		in al , dx
  		mov VAL,al

ret
recChar endp
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;   Main   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


MAIN PROC FAR    
MOV AX,@DATA 
MOV DS,AX
;Change to text Mode
Mov Ah,0
Mov Al,03h
INT 10H

Call InitializationSerial

CALL FirstS ;The first screen to input Name & points
Clear
Call SecondS ;The Second screen (Main)
;Call ChatMode
;Call GMainScreen

BYE:
Clear
Main ENDP
END Main