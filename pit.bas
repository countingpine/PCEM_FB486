/'IBM AT -
  Write B0
  Write aa55
  Expects aa55 back'/
'include <string.h>
'include "ibm.h"
'include "pit.h"
'include "video.h"
'include "cpu.h"
/'B0 to 40, two writes to 43, then two reads - value does not change!'/
/'B4 to 40, two writes to 43, then two reads - value _does_ change!'/
Dim Shared as Integer displine
Dim Shared as Integer pitsec=0
Dim Shared as Double PITCONST
Dim Shared as Single cpuclock
Dim Shared as Single isa_timing, bus_timing
Dim Shared as Integer firsttime=1
Sub setpitclock(clock As Single ) 
        Dim As Single temp 
        cpuclock=clock 
        PITCONST=clock/1193182.0 
        SPKCONST=clock/48000.0 
        CGACONST=(clock/(19687500.0/11.0)) 
        MDACONST=(clock/1813000.0) 
        VGACONST1=(clock/25175000.0) 
        VGACONST2=(clock/28322000.0) 
        SOUNDCONST=clock/200.0 
        CASCONST=PITCONST*1192 
        isa_timing = clock/8000000.0 
        bus_timing = clock/(double)cpu_busspeed 
        video_updatetiming() 
        GUSCONST=(clock/3125.0)/4.0 
        GUSCONST2=(clock/3125.0)/4.0: ''Timer 2 at different rate to 1?
        video_recalctimings() 
        RTCCONST=clock/32768.0 
End Sub
Dim Shared as Integer pit0
Sub pit_reset() 
        memset( And pit,0,sizeof(PIT)) 
        pit.l(0)=&hFFFF: pit.c(0)=&hFFFF*PITCONST 
        pit.l(1)=&hFFFF: pit.c(1)=&hFFFF*PITCONST 
        pit.l(2)=&hFFFF: pit.c(2)=&hFFFF*PITCONST 
        pit.m(0)=pit.m(1)=pit.m(2)=0 
        pit.ctrls(0)=pit.ctrls(1)=pit.ctrls(2)=0 
        pit.thit(0)=1 
        spkstat=0 
End Sub
Sub clearpit() 
        pit.c(0)=(pit.l(0) Shl 2) 
End Sub
Function pit_timer0_freq() As Single 
        return 1193182.0f/(float)pit.l(0) 
End Function
Dim Shared as Integer ins
Sub pit_write(addr As UShort , val As UByte ) 
        Dim As Integer t 
        Dim As UByte  oldctrl=pit.ctrl 
        cycles -= (int)PITCONST 
        pit0=1 
        Select Case  (addr And 3)
                case 3  /'CTRL'/
                if ((val And &hC0)=&hC0) Then 
                        if ( Not (val And &h20)) Then 
                                if (val And 2) Then pit.rl(0)=pit.c(0)/PITCONST 
                                if (val And 4) Then pit.rl(1)=pit.c(1)/PITCONST 
                                if (val And 8) Then pit.rl(2)=pit.c(2)/PITCONST 
                        End If
                        return 
                End If
                pit.ctrls(val Shr 6)=pit.ctrl=val 
                if ((val Shr 7)=3) Then 
                        printf("Bad PIT reg select _n") 
                        return 
                End If
                if ( Not (pit.ctrl And &h30)) Then 
                        pit.rl(val Shr 6)=pit.c(val Shr 6)/PITCONST 
                        if (pit.c(val Shr 6)<0) Then pit.rl(val Shr 6)=0 
                        pit.ctrl = ctrl Or &h30 
                        pit.rereadlatch(val Shr 6)=0 
                        pit.rm(val Shr 6)=3 
                else
                                pit.rm(val Shr 6)=pit.wm(val Shr 6)=(pit.ctrl Shr 4) And 3 
                                pit.m(val Shr 6)=(val Shr 1) And 7 
                                if (pit.m(val Shr 6)>5) Then pit.m(val Shr 6) = 6) And 3 
                                if ( Not (pit.rm(val Shr 6))) Then 
                                        pit.rm(val Shr 6)=3 
                                        pit.rl(val Shr 6)=pit.c(val Shr 6)/PITCONST 
                                End If
                                pit.rereadlatch(val Shr 6)=1 
                                if ((val Shr 6)=2) Then ppispeakon=speakon=(pit.m(2)=0),0,1 
                End If
                pit.wp=0 
                pit.thit(pit.ctrl Shr 6)=0 
                break 
                case 0, 1, 2  /'Timers'/
                t=addr And 3 
                Select Case  (pit.wm(t))
                        case 1 
                        pit.l(t)=val 
                        pit.thit(t)=0 
                        pit.c(t)=pit.l(t)*PITCONST 
                        picintc(1) 
                        break 
                        case 2 
                        pit.l(t)=(val Shl 8) 
                        pit.thit(t)=0 
                        pit.c(t)=pit.l(t)*PITCONST 
                        picintc(1) 
                        break 
                        case 0 
                        pit.l(t) = t) And &hFF 
                        pit.l(t) = t) Or (val Shl 8) 
                        pit.c(t)=pit.l(t)*PITCONST 
                        pit.thit(t)=0 
                        pit.wm(t)=3 
                        picintc(1) 
                        break 
                        case 3 
                        pit.l(t) = t) And &hFF00 
                        pit.l(t) = t) Or val 
                        pit.wm(t)=0 
                        break 
/'
                        if (pit.wp)
                        {
                                pit.l(t)&=0xFF;
                                pit.l(t)|=(val<<8);
                                pit.c(t)=pit.l(t)*PITCONST;
                                pit.thit(t)=0;
                        }
                        else
                        {
                                pit.l(t)&=0xFF00;
                                pit.l(t)|=val;
                        }
                        pit.rl(t)=pit.l(t);
                        pit.wp^=1;
                        pit.rm(t)=3;
                                pit.rereadlatch(t)=1;
                        break;'/
               End Select
               speakval=(((float)pit.l(2)/(float)pit.l(0))*&h4000)-&h2000 
                if (speakval>&h2000) Then speakval=&h2000 
                if ( Not pit.l(t)) Then 
                        pit.l(t) = t) Or &h10000 
                        pit.c(t)=pit.l(t)*PITCONST 
                End If
                break 
       End Select
nd Sub
Function pit_read(addr As UShort ) As UByte 
        Dim As UByte  temp 
        cycles -= (int)PITCONST         
        Select Case  (addr And 3)
                case 0, 1, 2  /'Timers'/
                if (pit.rereadlatch(addr And 3)) Then 
                        pit.rereadlatch(addr And 3)=0 
                        pit.rl(addr And 3)=pit.c(addr And 3)/PITCONST 
                        if ((pit.c(addr And 3)/PITCONST)>65536) Then pit.rl(addr And 3)=&hFFFF 
                End If
                Select Case  (pit.rm(addr And 3))
                        case 0 
                        temp=pit.rl(addr And 3) Shr 8 
                        pit.rm(addr And 3)=3 
                        pit.rereadlatch(addr And 3)=1 
                        break 
                        case 1 
                        temp=(pit.rl(addr And 3)) And &hFF 
                        pit.rereadlatch(addr And 3)=1 
                        break 
                        case 2 
                        temp=(pit.rl(addr And 3)) Shr 8 
                        pit.rereadlatch(addr And 3)=1 
                        break 
                        case 3 
                        temp=(pit.rl(addr And 3)) And &hFF 
                        if (pit.m(addr And 3) And &h80) Then 
                                pit.m(addr And 3) = 3) And 7 
                        else
                                pit.rm(addr And 3)=0
                        End If
                        break 
               End Select
               break 
                case 3  /'Control'/
                temp=pit.ctrl 
       End Select
'        printf("%02X %i %i %04X:%04X\n",temp,pit.rm(addr&3),pit.wp,cs>>4,pc);
        return temp 
End Function
Sub pit_poll() 
        pitsec+=1 
        if (pit.c(0)<1) Then 
                if (pit.m(0)=0  Or  pit.m(0)=4) Then 
                        pit.c(0)+=(&h10000*PITCONST) 
                else if (pit.m(0)=3  Or  pit.m(0)=2) Then
                        if (pit.l(0)) Then 
                                pit.c(0)+=((float)(pit.l(0)*PITCONST)) 
                        else
                                      pit.c(0)+=((float)(&h10000*PITCONST))
                     End If
                        End If
                End If
                if ( Not pit.thit(0)  And  (pit.l(0)>&h14)) Then 
                        picint(1) 
                End If
                if ( Not pit.m(0)  Or  pit.m(0)=4) Then pit.thit(0)=1 
                pit0=0 
                pitcount+=1 
        End If
        if (pit.c(1)<1) Then 
                if (pit.m(1)=0  Or  pit.m(1)=4) Then 
                        pit.c(1)=&hFFFFFF*PITCONST 
                else
                        pit.c(1)+=(pit.l(1)*PITCONST) 
                End If
                readdma0() 
        End If
        if (pit.c(2)<1) Then 
                if ( Not pit.m(2)  Or  pit.m(2)=4) Then 
                        pit.c(2)+=(&h10000*PITCONST) 
                        speakon = speakon Xor 1 
                        ppispeakon = ppispeakon Xor 1 
                else
                        pit.c(2)+=((pit.l(2)*PITCONST)/2) 
                        if (pit.l(2)>&h30) Then /'Some games use very high frequencies as 'speaker off'. This stops them from generating noise'/speakon = speakon Xor 1 
                        ppispeakon = ppispeakon Xor 1 
                End If
        End If
End Sub
Sub pit_init() 
        io_sethandler(&h0040, &h0004, pit_read, NULL, NULL, pit_write, NULL, NULL) 
End Sub
