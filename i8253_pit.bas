'IBM AT -
'  Write B0
'  Write aa55
'  Expects aa55 back
  
'B0 to 40, two writes to 43, then two reads - value does not change!
'B4 to 40, two writes to 43, then two reads - value _does_ change!

static shared As Integer pitsec=0
static shared As Double PITCONST



Sub setpitclock(clock As Single ) 
        Dim As Single temp =Any
        
        CPUCLOCK = clock 
        PITCONST = clock/1193182.0
        RTCCONST = clock/32768.0
        VGACONST1= clock/25175000.0 
        VGACONST2= clock/28322000.0 

			' isa=8.33
			' bus=2.00
        isa_timing   = clock/8000000.0  
        bus_timing   = clock/33333333.0 ' 33mhz BUS del 486dx (66mhz/2) (cpu_multi=2, multiplicador x2 del DX2)
                
        video_updatetiming()
        svga_recalctimings()
End Sub


Sub pit_reset() 
        pit.l(0)=&hFFFF
        pit.c(0)=&hFFFF*PITCONST 
        pit.l(1)=&hFFFF
        pit.c(1)=&hFFFF*PITCONST 
        pit.l(2)=&hFFFF
        pit.c(2)=&hFFFF*PITCONST 
        
        pit.m(0)=0
        pit.m(1)=0
        pit.m(2)=0 
        
        pit.ctrls(0)=0
        pit.ctrls(1)=0
        pit.ctrls(2)=0 
        
        pit.thit(0)=1 
End Sub


Sub pit_write(ByVal addr As UShort , ByVal valor As UByte ) 
        Dim As Integer t =any
        Dim As UByte  oldctrl=pit.ctrl 
        cycles -= PITCONST 
        Select Case As Const  (addr And 3)
        	Case 3  /'CTRL'/
                if ((valor And &hC0)=&hC0) Then 
                     if (valor And &h20)=0 Then 
                       if (valor And 2) Then pit.rl(0)=pit.c(0)/PITCONST 
                       if (valor And 4) Then pit.rl(1)=pit.c(1)/PITCONST 
                       if (valor And 8) Then pit.rl(2)=pit.c(2)/PITCONST 
                     EndIf
                     return 
                EndIf
                pit.ctrls(valor Shr 6)=valor
                pit.ctrl=valor 
                if (valor Shr 7)=3 Then 
                        'if deb=3 then print #5,"Bad PIT reg select"
                        return 
                EndIf
                if (pit.ctrl And &h30)=0 Then 
                        pit.rl(valor Shr 6)=pit.c(valor Shr 6)/PITCONST 
                        if pit.c(valor Shr 6)<0 Then pit.rl(valor Shr 6)=0 
                        pit.ctrl = pit.ctrl Or &h30 
                        pit.rereadlatch(valor Shr 6)=0 
                        pit.rm(valor Shr 6)=3 
                else
                       pit.rm(valor Shr 6)=(pit.ctrl Shr 4) And 3 
                       pit.wm(valor Shr 6)=(pit.ctrl Shr 4) And 3 
                       pit.m (valor Shr 6)=(valor Shr 1) And 7 
                       if pit.m(valor Shr 6)>5 Then pit.m(valor Shr 6) And = 3 
                       if (pit.rm(valor Shr 6))=0 Then 
                               pit.rm(valor Shr 6)=3 
                               pit.rl(valor Shr 6)=pit.c(valor Shr 6)/PITCONST 
                       EndIf
                       pit.rereadlatch(valor Shr 6)=1 
                EndIf
                pit.wp=0 
                pit.thit(pit.ctrl Shr 6)=0 
                 
        	Case 0, 1, 2  /'Timers'/
                t=addr And 3 
                Select Case As Const  (pit.wm(t))              	                         
                	Case 0 
                        pit.l(t) And=&hFF 
                        pit.l(t) Or =(valor Shl 8) 
                        pit.c(t)=pit.l(t)*PITCONST 
                        pit.thit(t)=0 
                        pit.wm(t)=3 
                        picintc(1)
                         
                	Case 1 
                        pit.l(t)=valor 
                        pit.thit(t)=0 
                        pit.c(t)=pit.l(t)*PITCONST 
                        picintc(1) 
                         
                	Case 2 
                        pit.l(t)=(valor Shl 8) 
                        pit.thit(t)=0 
                        pit.c(t)=pit.l(t)*PITCONST 
                        picintc(1) 
                        
                	Case 3 
                        pit.l(t) And=&hFF00 
                        pit.l(t) Or =valor 
                        pit.wm(t)=0 

                End Select
                
                if pit.l(t)=0 Then 
                        pit.l(t) or=&h10000 
                        pit.c(t)=pit.l(t)*PITCONST 
                EndIf
                 
        End Select
end Sub

Function pit_read(ByVal addr As UShort ) As UByte 
        Dim As UByte temp =Any
        cycles -= PITCONST       
        
        Select Case As Const  (addr And 3)
        	Case 0, 1, 2  /'Timers'/
                if pit.rereadlatch(addr And 3) Then 
                        pit.rereadlatch(addr And 3)=0 
                        pit.rl(addr And 3)=pit.c(addr And 3)/PITCONST 
                        if (pit.c(addr And 3)/PITCONST)>65535 Then pit.rl(addr And 3)=&hFFFF 
                EndIf
                Select Case As Const  (pit.rm(addr And 3))
                	Case 0 
                        temp=pit.rl(addr And 3) Shr 8 
                        pit.rm(addr And 3)=3 
                        pit.rereadlatch(addr And 3)=1 
                         
                	Case 1 
                        temp=(pit.rl(addr And 3)) And &hFF 
                        pit.rereadlatch(addr And 3)=1 
                         
                	Case 2 
                        temp=(pit.rl(addr And 3)) Shr 8 
                        pit.rereadlatch(addr And 3)=1 
                         
                	Case 3 
                        temp=(pit.rl(addr And 3)) And &hFF 
                        if pit.m(addr And 3) And &h80 Then 
                           pit.m(addr And 3) and= 7 
                        else
                           pit.rm(addr And 3)=0
                        EndIf
                        
                End Select
                
        	Case 3  /'Control'/
                temp=pit.ctrl 
        End Select

        return temp 
End Function

Sub pit_poll() 
        pitsec+=1 

        if pit.c(0)<1 Then 
                if (pit.m(0)=0) Or (pit.m(0)=4) Then 
                        pit.c(0)+=(&h10000*PITCONST) 
                ElseIf (pit.m(0)=3) Or (pit.m(0)=2) Then
                        if pit.l(0) Then 
                            pit.c(0)+=CDbl(pit.l(0)*PITCONST) ' me parece mas correcto con CDBL
                        Else
                            pit.c(0)+=CDbl(&h10000*PITCONST)
                        EndIf
                EndIf
                if (pit.thit(0)=0) And (pit.l(0)>&h14) Then picint(1) 
                if (pit.m(0)=0) Or (pit.m(0)=4) Then pit.thit(0)=1 
        EndIf
        if pit.c(1)<1 Then 
                if (pit.m(1)=0) Or (pit.m(1)=4) Then 
                        pit.c(1)=(&hFFFFFF*PITCONST) 
                else
                        pit.c(1)+=(pit.l(1)*PITCONST) 
                EndIf
                readdma0() 
        EndIf
        if pit.c(2)<1 Then 
                if (pit.m(2)=0) Or (pit.m(2)=4) Then 
                        pit.c(2)+=(&h10000*PITCONST) 
                else
                        pit.c(2)+=((pit.l(2)*PITCONST)/2) 
                EndIf
        EndIf
End Sub
