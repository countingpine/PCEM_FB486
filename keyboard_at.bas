

const STAT_PARITY     =&h80
Const STAT_RTIMEOUT   =&h40
Const STAT_TTIMEOUT   =&h20
Const STAT_MFULL      =&h20
Const STAT_LOCK       =&h10
Const STAT_CD         =&h08
Const STAT_SYSFLAG    =&h04
Const STAT_IFULL      =&h02
Const STAT_OFULL      =&h01

type structkeyb
		  As Integer initialised
        As Integer want60
        As Integer wantirq
        As Integer wantirq12
        As Integer key_wantdata
        as UByte command0
        As UByte status
        As UByte mem(0 To 31)
        As UByte out0
        As UByte input_port
        As UByte output_port
        As UByte key_command
End Type
static shared keyboard_at As structkeyb


static Shared as UByte key_ctrl_queue(0 To 15)
Static Shared as Integer key_ctrl_queue_start=  0, key_ctrl_queue_end = 0
Static Shared as UByte key_queue(0 To 15)
Static Shared as Integer key_queue_start = 0, key_queue_end = 0
Static Shared as uByte mouse_queue(0 To 15)

static shared As Integer mouse_queue_start = 0, mouse_queue_end = 0

static shared As Integer keyboard_scan
static shared As Integer mouse_write


'Declare Sub keyboard_at_adddata(valor As ubyte)
'Declare Sub keyboard_at_adddata_keyboard(valor As ubyte)
Sub keyboard_at_poll() 
	
     If (keyboard_at.wantirq) Then 
             keyboard_at.wantirq = 0 
             picint(2) 
             ' 'if deb=3 then print #5,"keyboard_at   take IRQ ..............."
     ElseIf (keyboard_at.wantirq12) Then
				 keyboard_at.wantirq12 = 0
             picint(&h1000) 
             ' 'if deb=3 then print #5,"keyboard_at   take IRQ 12 ..............."
     EndIf
     
     If ((keyboard_at.status And STAT_OFULL)=0) And ((keyboard_at.mem(0) And &h10)=0) And (mouse_queue_start <> mouse_queue_end) Then 
             ' 'if deb=3 then print #5,"Reading from the mouse queue at ", keyboard_at.out0, key_queue_start 
             keyboard_at.out0    = mouse_queue(mouse_queue_start) 
             mouse_queue_start   = (mouse_queue_start + 1) And &hf 
             keyboard_at.status  = keyboard_at.status Or (STAT_OFULL Or STAT_MFULL) 
             keyboard_at.status  = keyboard_at.status And inv(STAT_IFULL) 
             if (keyboard_at.mem(0)  And &h02) Then keyboard_at.wantirq12 = 1         
     ElseIf ((keyboard_at.status And STAT_OFULL)=0) And ((keyboard_at.mem(0) And &h10)=0) And (key_queue_start <> key_queue_end) Then
				 ' 'if deb=3 then print #5,"Reading from the key queue at ", keyboard_at.out0, key_queue_start 
             keyboard_at.out0    = key_queue(key_queue_start) 
             key_queue_start     = (key_queue_start + 1) And &hf 
             keyboard_at.status  = keyboard_at.status Or  STAT_OFULL 
             keyboard_at.status  = keyboard_at.status And inv(STAT_IFULL) 
             if (keyboard_at.mem(0) And &h01) Then keyboard_at.wantirq = 1     
     ElseIf ((keyboard_at.status And STAT_OFULL)=0) And (key_ctrl_queue_start <> key_ctrl_queue_end) Then 
             ' 'if deb=3 then print #5,"Reading from the key ctrl_queue at ", keyboard_at.out0, key_ctrl_queue_start 
             keyboard_at.out0     = key_ctrl_queue(key_ctrl_queue_start) 
             key_ctrl_queue_start = (key_ctrl_queue_start + 1) And &hf 
             keyboard_at.status   = keyboard_at.status Or  STAT_OFULL 
             keyboard_at.status   = keyboard_at.status And inv(STAT_IFULL) 
             if (keyboard_at.mem(0) And &h01) Then keyboard_at.wantirq = 1         
     EndIf

End Sub

Sub keyboard_at_adddata(valor As UByte ) 
       key_ctrl_queue(key_ctrl_queue_end) = valor 
       key_ctrl_queue_end = (key_ctrl_queue_end + 1) And &hf 
       ' 'if deb=3 then print #5,"keyboard_at added to queue ", valor                 
End Sub

Sub keyboard_at_adddata_keyboard(valor As UByte ) 
        key_queue(key_queue_end) = valor 
        key_queue_end = (key_queue_end + 1)  And &hf 
        ' 'if deb=3 then print #5,"keyboard_at added to key queue ", valor 
        return 
End Sub

Sub keyboard_at_adddata_mouse(valor As UByte ) 
        mouse_queue(mouse_queue_end) = valor 
        mouse_queue_end = (mouse_queue_end + 1) And &hf 
        ' 'if deb=3 then print #5,"keyboard_at added to mouse queue ", valor 
        return 
End Sub

Sub keyboard_at_write(port As UShort , valor As UByte ) 
        ' 'if deb=3 then print #5,"keyboard_at  write ", port, valor, keyboard_at.key_wantdata, ram[8] 

        Select Case As Const  (port)
        	Case &h60 
                if (keyboard_at.want60) Then 
                        /'Write to controller'/
                        keyboard_at.want60 = 0 
                        Select Case As Const  (keyboard_at.command0)
                        	Case  &h60, &h61, &h62, &h63, _
                        	 		&h64, &h65, &h66, &h67, _
                        	 		&h68, &h69, &h6a, &h6b, _
                        	 		&h6c, &h6d, &h6e, &h6f, _
                        	 		&h70, &h71, &h72, &h73, _
                        	 		&h74, &h75, &h76, &h77, _
                        	 		&h78, &h79, &h7a, &h7b, _
                        	 		&h7c, &h7d, &h7e, &h7f 
                                keyboard_at.mem(keyboard_at.command0  And &h1f) = valor 
                                if (keyboard_at.command0 = &h60) Then 
                                        if ((valor And 1)<>0) And ((keyboard_at.status And STAT_OFULL)<>0) Then keyboard_at.wantirq = 1 
                                        if ((valor And 1)= 0) And  (keyboard_at.wantirq<>0) Then keyboard_at.wantirq = 0 
                                EndIf
                                 
                        	case &hcb  /'AMI - set keyboard mode'/
                                Exit Select ' nada
                                 
                        	case &hcf  /'??? - sent by MegaPC BIOS'/
                                Exit Select ' nada
                                 
                        	case &hd1  /'Write output port'/
                                ' 'if deb=3 then print #5,"Write output port  ", keyboard_at.output_port, valor, CS1, pc 
                                if ((keyboard_at.output_port Xor valor) And &h02) Then 
                                        /'A20 enable change'/
                                        mem_a20_key = valor And &h02 
                                        mem_a20_recalc() 
                                        flushmmucache() 
                                EndIf
                                keyboard_at.output_port = valor 
                                 
                        	case &hd3  /'Write to mouse output buffer'/
                                keyboard_at_adddata_mouse(valor) 
                                 
                        	'case &hd4  /'Write to mouse'/
                                'if (mouse_write) Then 
                                	' 'if deb=3 then print #5,"escribir a raton, sin hacer" 
                                	'mouse_write(valor) 
                                'EndIf
                                      
                        	'Case Else 
                                ' 'if deb=3 then print #5,"Bad AT keyboard controller 0060 write ", valor, keyboard_at.command0 
                       End Select
                else
                        /'Write to keyboard'/                        
                        keyboard_at.mem(0) = keyboard_at.mem(0) And inv(&h10)
                        if (keyboard_at.key_wantdata) Then 
                                keyboard_at.key_wantdata = 0 
                                Select Case As Const  (keyboard_at.key_command)
                                	Case &hed  /'Set/reset LEDs'/
                                        keyboard_at_adddata_keyboard(&hfa) 
                                         
                                	Case &hf3  /'Set typematic rate/delay'/
                                        keyboard_at_adddata_keyboard(&hfa) 
                               End Select
                        Else
                                keyboard_at.key_command = valor 
                                Select Case As Const  (valor)
                                	Case &h05  /'??? - sent by NT 4.0'/
                                        keyboard_at_adddata_keyboard(&hfe) 
                                         
                                	Case &hed  /'Set/reset LEDs'/
                                        keyboard_at.key_wantdata = 1 
                                        keyboard_at_adddata_keyboard(&hfa) 
                                         
                                	case &hf2  /'Read ID'/
                                        keyboard_at_adddata_keyboard(&hfa) 
                                        keyboard_at_adddata_keyboard(&hab) 
                                        keyboard_at_adddata_keyboard(&h41) 
                                         
                                	case &hf3  /'Set typematic rate/delay'/
                                        keyboard_at.key_wantdata = 1 
                                        keyboard_at_adddata_keyboard(&hfa) 
                                         
                                	case &hf4  /'Enable keyboard'/
                                        keyboard_scan = 1 
                                         
                                	case &hff  /'Reset'/
                                        keyboard_at_adddata_keyboard(&hfa) 
                                        keyboard_at_adddata_keyboard(&haa) 
                                         
                                	Case Else 
                                        ' 'if deb=3 then print #5,"Bad AT keyboard command  ", valor 
                                        keyboard_at_adddata_keyboard(&hfe) 
                               End Select
                        EndIf
                EndIf
                 
        	Case &h61 
                ppi.pb=valor 

        	Case &h64 
                keyboard_at.want60 = 0 
                keyboard_at.command0 = valor 
                /'New controller command'/
                Select Case As Const  (valor)
                	Case  &h20, &h21, &h22, &h23, _
                	 		&h24, &h25, &h26, &h27, _
                	 		&h28, &h29, &h2a, &h2b, _
                	 		&h2c, &h2d, &h2e, &h2f, _
                	 		&h30, &h31, &h32, &h33, _
                	 		&h34, &h35, &h36, &h37, _
                	 		&h38, &h39, &h3a, &h3b, _
                	 		&h3c, &h3d, &h3e, &h3f 
                        keyboard_at_adddata(keyboard_at.mem(valor And &h1f))
                         
                	case  &h60, &h61, &h62, &h63, _
                	 		&h64, &h65, &h66, &h67, _
                	 		&h68, &h69, &h6a, &h6b, _
                	 		&h6c, &h6d, &h6e, &h6f, _
                	 		&h70, &h71, &h72, &h73, _
                	 		&h74, &h75, &h76, &h77, _
                	 		&h78, &h79, &h7a, &h7b, _
                	 		&h7c, &h7d, &h7e, &h7f 
                        keyboard_at.want60 = 1 
                         
                	case &ha1  /'AMI - get controlled version'/
                        Exit select ' nada
                         
                	case &ha7  /'Disable mouse port'/
                        Exit select ' nada
                         
                	case &ha9  /'Test mouse port'/
                        keyboard_at_adddata(&h00)  /'no error'/
                         
                	case &haa  /'Self-test'/
                        if keyboard_at.initialised=0 Then 
                                keyboard_at.initialised = 1 
                                key_ctrl_queue_start = 0
                                key_ctrl_queue_end  = 0 
                                keyboard_at.status  = keyboard_at.status  And  inv(STAT_OFULL) 
                        EndIf
                        keyboard_at.status  = keyboard_at.status  Or STAT_SYSFLAG 
                        keyboard_at.mem(0)  = keyboard_at.mem(0)  Or &h04 
                        keyboard_at_adddata(&h55) 
                         
                	case &hab  /'Interface test'/
                        keyboard_at_adddata(&h00) /'no error'/
                         
                	case &had  /'Disable keyboard'/
                        keyboard_at.mem(0) = keyboard_at.mem(0) Or &h10 
                         
                	case &hae  /'Enable keyboard'/
                        keyboard_at.mem(0) = keyboard_at.mem(0) And inv(&h10) 
                         
                	case &hc0  /'Read input port'/
                        keyboard_at_adddata(keyboard_at.input_port) 
                        keyboard_at.input_port = ((keyboard_at.input_port + 1)  And 3)  Or  (keyboard_at.input_port  And &hfc) 
                         
                	case &hc9  /'AMI - block P22 and P23 ??, '/
                        Exit select ' nada
                         
                	case &hca /'AMI - read keyboard mode'/
                        keyboard_at_adddata(&h00): /'ISA mode'/
                         
                	case &hcb  /'AMI - set keyboard mode'/
                        keyboard_at.want60 = 1 
                         
                	case &hcf  /'??? - sent by MegaPC BIOS'/
                        keyboard_at.want60 = 1 
                         
                	case &hd0  /'Read output port'/
                        keyboard_at_adddata(keyboard_at.output_port) 
                         
                	case &hd1  /'Write output port'/
                        keyboard_at.want60 = 1 
                         
                	case &hd3  /'Write mouse output buffer'/
                        keyboard_at.want60 = 1 
                         
                	case &hd4  /'Write to mouse'/
                        keyboard_at.want60 = 1 
                         
                	case &he0  /'Read test inputs'/
                        keyboard_at_adddata(&h00) 
                         
                	case &hef  /'??? - sent by AMI486'/
                        Exit Select ' nada
                        
                	case &hfe  /'Pulse output port - pin 0 selected - x86 reset'/
                        softresetx86(): /'Pulse reset!'/
                         
                	'case &hff  /'Pulse output port - but no pins selected - sent by MegaPC BIOS'/
                        ' nada
                        
                	'Case Else 
                        ' 'if deb=3 then print #5,"Bad AT keyboard controller command ", valor 
               End Select
       End Select
end Sub

Function keyboard_at_read(port As UShort ) As UByte 
        Dim As UByte temp = &hff 
        cycles -= 4 
        Select Case As Const(port)
        	Case &h60 
                temp = keyboard_at.out0 
                keyboard_at.status  = keyboard_at.status And inv(STAT_OFULL Or STAT_MFULL) 
                keyboard_at.wantirq = 0
                keyboard_at.wantirq12 = 0 
                 
        	Case &h61                 
                'if (ppispeakon) Then return (ppi.pb And inv(&hC0)) Or &h20 
                return ppi.pb And inv(&hC0)
                 
        	Case &h64 
                temp = keyboard_at.status 
                keyboard_at.status = keyboard_at.status And inv(STAT_RTIMEOUT Or STAT_TTIMEOUT) 
        End Select

        return temp 
End Function

Sub keyboard_at_reset() 
	
		  mouse_write = 0 'NULL 
		  
        keyboard_at.initialised = 0 
        keyboard_at.status = STAT_LOCK Or STAT_CD 
        keyboard_at.mem(0) = &h11 
        keyboard_at.wantirq = 0 
        keyboard_at.output_port = 0 
        keyboard_at.input_port = &hb0 
        keyboard_at.key_wantdata = 0 
        keyboard_scan = 1 
End Sub

