Dim Shared as ULong x87_pc_off
Dim Shared as ULong x87_op_off

Dim Shared as UShort x87_pc_seg
Dim Shared as UShort x87_op_seg

Declare Sub x87_d8()
Declare Sub x87_d9()
Declare Sub x87_da()
Declare Sub x87_db()
Declare Sub x87_dc()
Declare Sub x87_dd()
Declare Sub x87_de()
Declare Sub x87_df()

Dim Shared as Double STD(8)
Dim Shared as UShort npxs,npxc,tag
Dim Shared as integer TOP

#Define ST(x) (STD((TOP+(x)) And 7))

#Define C0 (1 Shl 8)
#Define C1 (1 Shl 9)
#Define C2 (1 Shl 10)
#Define C3 (1 Shl 14)

#Define BIAS80 16383
#define BIAS64 1023


		

'rutinas en 386A.C necesarias aqui
Declare Sub seteab(v As Ubyte) 
Declare Sub seteaw(v As UShort) 
Declare Sub seteal(v As ULong)
Declare Function geteab() As UByte 
Declare Function geteaw() As UShort 
Declare Function geteal() As uLong 

' esta rutina requiere la matematica MATH.BI para las dos funciones FLOOR y CEIL (y POW al parecer)
' FLOOR
' devuelve el entero anterior al valor indicado
' ejemplos: 2.5=2, -2.3=-3
'
' CEIL:
' devuelve el entero posterior al valor indicado
' ejemplos: 2.5=3, -2.3=-2
#Include "crt\math.bi"
Function x87_fround(b As Double ) As LongInt 
       Select Case As Const  ((npxc Shr 10) And 3)
       	Case 0  'Nearest
       	' este falla en BASIC, lo redondea para arriba, y deberia ser para abajo
       	' por eso, como truco, lo hago entero (FIX) y asi, redondea para abajo
       	' sin est truco, falla mi "D.EXE"
                return CLngInt(Fix(b)+0.5) 
       	Case 1  'Down
                Return CLngInt(floor(b)) 'floor=double
       	Case 2  'Up
                Return CLngInt(ceil(b)) ' ceil=double
       	Case 3  'Chop
       	       ' este falla en el WOLFSTEIN3D si NO es INT!!!!
       	       ' pero a su vez, falla el TEST387 si ES INT!!!!  Que hago?
       	       ' parece que FIX soluciona el tema!!
                return clngint(fix(b)) 
       End Select
End Function

' no funciona bien, la anulo por ahora.
#define STATUS_ZERODIVIDE 4
Function x87_div(ByRef dst As double, src1 As double, src2 As Double) As Integer  
	'Do
          'If src2 = 0.0 Then                                      
          '  npxs = npxs Or STATUS_ZERODIVIDE
            'if npxc and STATUS_ZERODIVIDE Then
            '    dst = src1 / src2
            'else                                                          
          '      picint(1 Shl 13)                                       
            'End if    
           ' Return 1 ' si sale con "1", en la rutina que lo ha llamado se hace un RETURN (salida de sub)
          'Else  
          ' no funciona bien esta zona, por eso, dejo solo la parte divisora "normal"
          ' se nota que falla en el test 287demo, en el CAD SHUTTLE, zona del motor.
            dst = src1 / src2
          'End If
	'Loop while 0
   Return 0
End Function 


Sub x87_push(i As double ) 
        TOP=(TOP-1) And 7 
        STD(TOP)=i 
        tag = tag And inv(3 Shl ((TOP And 7) Shl 1))
        if (i=0.0) Then tag = tag Or (1 Shl ((TOP And 7) Shl 1)) 
End Sub

Function x87_pop() As Double 
        Dim As Double t=STD(TOP) 
        tag = tag Or (3 Shl ((TOP And 7) Shl 1)) 
        TOP=(TOP+1) And 7 
        return t 
End Function


Function x87_ld80() As Double 
	
	Type test0
	    As Short begin
	    Union
	       As Double eind_d
	       As ULongInt eind_ll
		 End Union
	End Type 
	Dim test As test0

	test.eind_ll = readmeml_386(easeg,eaaddr) 
	test.eind_ll = test.eind_ll Or (CULngInt(readmeml_386(easeg,eaaddr+4)) Shl 32) 
	test.begin   = readmemw_386(easeg,eaaddr+8) 
	
	Dim As LongInt  exp64 = (((test.begin And &h7fff) - BIAS80)) 
	Dim As LongInt  blah = IIf((exp64>0),exp64,-exp64) And &h3ff 
	Dim As LongInt  exp64final = iif((exp64>0),blah,-blah) +BIAS64 
	
	Dim As LongInt  mant64 = (test.eind_ll Shr 11) And &hfffffffffffff 
	Dim As LongInt  sign = IIf((test.begin And &h8000),1,0) 

   If (test.begin And &h7fff) = &h7fff Then exp64final = &h7ff ' nuevo en la 9.1
   If (test.begin And &h7fff) = 0 Then exp64final = 0	 ' nuevo en la 9.1
   If test.eind_ll And &h400 Then mant64+=1 
   
	test.eind_ll = (sign Shl 63) Or (exp64final Shl 52) Or mant64 
	
	return test.eind_d
	
End Function

Sub x87_st80(d As Double ) 
	
	Type test0
	    As Short begin
	    Union
	       As Double eind_d
	       As ULongInt eind_ll
		 End Union
	End Type 
	Dim test As test0
	
	test.eind_d=d 
	
	Dim As LongInt sign80 = IIf((test.eind_ll And &h8000000000000000),1,0)
	Dim As LongInt exp80 = test.eind_ll And &h7ff0000000000000
	Dim As LongInt exp80final = (exp80 Shr 52) 
	Dim As LongInt mant80 = test.eind_ll And &h000fffffffffffff 
	Dim As LongInt mant80final = (mant80 Shl 11) 
	
	if exp80final = &h7ff Then ' Infinity/Nan
                exp80final = &h7fff
                mant80final Or= &h8000000000000000
	ElseIf d <> 0 Then 
		' Elvira wants the 8 and tcalc doesnt
		mant80final  = mant80final Or &h8000000000000000 
		' Ca-cyber doesnt like this when result is zero.
		exp80final += (BIAS80 - BIAS64) 
	EndIf
	
	test.begin = (CShort(sign80) Shl 15) Or CShort(exp80final) 
	test.eind_ll = mant80final 
	
	writememl_386(easeg,eaaddr,test.eind_ll And &Hffffffff) 
	writememl_386(easeg,eaaddr+4,test.eind_ll Shr 32) 
	writememw_386(easeg,eaaddr+8,test.begin) 
End Sub


Sub x87_d8()
	
	Union ts0 
		As single s 
		As ULong i 
	End Union
	Dim ts As ts0
   'Print #5,"D8:";modo,reg,Hex(rmdat32 And &hFF,2)
        if (modo=3) Then 
                Select Case As Const  (rmdat32 And &hFF)
                	Case &hC0, &hC1, &hC2, &hC3 ,&hC4, &hC5, &hC6, &hC7 'FADD
                        ST(0)=ST(0)+ST(rmdat32 And 7) 
                        cycles-=8 
                        return 
                	case &hC8, &hC9, &hCA, &hCB ,&hCC, &hCD, &hCE, &hCF 'FMUL
                        ST(0)=ST(0)*ST(rmdat32 And 7) 
                        cycles-=16 
                        return 
                	case &hD0, &hD1, &hD2, &hD3 ,&hD4, &hD5, &hD6, &hD7 'FCOM
                        npxs = npxs And inv(C0 Or C2 Or C3)
                        if ST(0)=ST(rmdat32 And 7) Then 
                           npxs = npxs Or C3 
                        elseif ST(0)<ST(rmdat32 And 7) Then
									npxs = npxs Or C0
                        EndIf
                        cycles-=4 
                        return 
                	case &hD8, &hD9, &hDA, &hDB ,&hDC, &hDD, &hDE, &hDF 'FCOMP
                        npxs = npxs And inv(C0 Or C2 Or C3)
                        if ST(0)=ST(rmdat32 And 7) Then 
                           npxs = npxs Or C3 
                        elseif ST(0)<ST(rmdat32 And 7) Then
									npxs = npxs Or C0
                        EndIf
                        x87_pop() 
                        cycles-=4 
                        return 
                	case &hE0, &hE1, &hE2, &hE3 ,&hE4, &hE5, &hE6, &hE7 'FSUB
                        ST(0)=ST(0)-ST(rmdat32 And 7) 
                        cycles-=8 
                        return 
                	case &hE8, &hE9, &hEA, &hEB ,&hEC, &hED, &hEE, &hEF 'FSUBR
                        ST(0)=ST(rmdat32 And 7)-ST(0) 
                        cycles-=8 
                        return 
                	case &hF0, &hF1, &hF2, &hF3 ,&hF4, &hF5, &hF6, &hF7 'FDIV
                        'ST(0)=ST(0)/ST(rmdat32 And 7)
                        If x87_div(ST(0),ST(0),ST(rmdat32 And 7)) Then Return ' version 9
                        cycles-=73 
                        return 
                	case &hF8, &hF9, &hFA, &hFB ,&hFC, &hFD, &hFE, &hFF 'FDIVR
                        'ST(0)=ST(rmdat32 And 7)/ST(0)
                        If x87_div(ST(0),ST(rmdat32 And 7),ST(0)) Then Return ' version 9
                        cycles-=73 
                        return 
                End Select
        else
                ts.i=geteal(): if abrt Then return 
                Select Case As Const (reg)
                	case 0  'FADD short
                        ST(0)+=ts.s 
                        cycles-=8 
                        return 
                	case 1  'FMUL short
                        ST(0)*=ts.s 
                        cycles-=11 
                        return 
                	case 2  'FCOM short
                        npxs = npxs And inv(C0 Or C2 Or C3)
                        if ST(0)=ts.s Then 
                           npxs = npxs Or C3 
                        ElseIf ST(0)<ts.s Then
									npxs = npxs Or C0
                        EndIf
                        cycles-=4 
                        return 
                	case 3  'FCOMP short
                        npxs = npxs And inv(C0 Or C2 Or C3)
                        if ST(0)=ts.s Then 
                           npxs = npxs Or C3 
                        ElseIf ST(0)<ts.s Then
									npxs = npxs Or C0
                        EndIf
                        cycles-=4 
                        x87_pop() 
                        return 
                	case 4  'FSUB short
                        ST(0)-=ts.s 
                        cycles-=8 
                        return 
                	case 5  'FSUBR short
                        ST(0)=ts.s-ST(0) 
                        cycles-=8 
                        return 
                	case 6  'FDIV short
                        'ST(0)=ST(0)/ts.s 
                        If x87_div(ST(0),ST(0),ts.s) Then Return ' version 9
                        cycles-=73 
                        return 
                	case 7  'FDIVR short
                        'ST(0)=ts.s/ST(0) 
                        If x87_div(ST(0),ts.s,ST(0)) Then return ' version 9
                        cycles-=73 
                        return 
                End Select
        EndIf
        x86illegal() 
End Sub
Sub x87_d9() 

	Union ts0 
		As single s 
		As ULong i 
	End Union
	Dim ts As ts0
   'Print #5,"D9:";modo,reg,Hex(rmdat32 And &hFF,2)
        Dim As Double td 
        Dim As LongInt temp64 
        Dim As UShort tempw 
        Dim As integer temp 

        if (modo=3) Then 
                Select Case As Const (rmdat32 And &hFF)
                	Case &hC0, &hC1, &hC2, &hC3 ,&hC4, &hC5, &hC6, &hC7 'FLD
                        x87_push(ST(rmdat32 And 7)) 
                        cycles-=4 
                        return 
                	case &hC8, &hC9, &hCA, &hCB ,&hCC, &hCD, &hCE, &hCF 'FXCH
                        td=ST(0) 
                        ST(0)=ST(rmdat32 And 7) 
                        ST(rmdat32 And 7)=td 
                        cycles-=4 
                        return 
                	case &hD0  'FNOP
                        cycles-=3 
                        return 
                	case &hD8, &hD9, &hDA, &hDB, &hDC, &hDD, &hDE, &hDF 'Invalid, but apparently not illega
                        ST(rmdat32 And 7) = ST(0)
                        temp = (tag Shr ((TOP And 7) Shl 1)) And 3
                        tag  = tag And inv(3 shl (((TOP + rmdat32) And 7) Shl 1))
                        tag  = tag Or (temp  Shl (((TOP + rmdat32) And 7) Shl 1))
                        x87_pop()
                        cycles-=3
                        Return
                	case &hE0  'FCHS
                        ST(0)=-ST(0) 
                        cycles-=6 
                        return 
                	case &hE1  'FABS
                        ST(0)=fabs(ST(0)) 
                        cycles-=3 
                        return 
                	case &hE4  'FTST
                        npxs = npxs And inv(C0 Or C2 Or C3)
                        If ST(0)=0.0 Then 
                           npxs = npxs Or C3 
                        ElseIf ST(0)<0.0 Then
									npxs = npxs Or C0
                        EndIf
                        cycles-=4 
                        return 
                	case &hE5  'FXAM
                        npxs = npxs And inv(C0 Or C2 Or C3)
                        if ((tag Shr ((TOP And 7) Shl 1)) And 3)=3 Then 
                              npxs = npxs Or (C0 Or C3) 
                        ElseIf ST(0)=0.0 Then
                              npxs = npxs Or C3
                        else
                              npxs = npxs Or C2
                        EndIf
                        if ST(0)<0.0 Then npxs = npxs Or C1 
                        cycles-=8 
                        return 
                	case &hE8  'FLD1
                        x87_push(1.0) 
                        cycles-=4 
                        return 
                	case &hE9  'FLDL2T
                        x87_push(3.3219280948873623) 
                        cycles-=8 
                        return 
                	case &hEA  'FLDL2E
                        x87_push(1.4426950408889634) 
                        cycles-=8 
                        return 
                	case &hEB  'FLDPI
                        x87_push(3.141592653589793) 
                        cycles-=8 
                        return 
                	case &hEC  'FLDEG2
                        x87_push(0.3010299956639812) 
                        cycles-=8 
                        return 
                	case &hED  'FLDLN2
                        x87_push(0.693147180559945) 
                        cycles-=8 
                        return 
                	case &hEE  'FLDZ
                        x87_push(0.0) 
                        tag = tag Or (1 Shl ((TOP And 7) Shl 1)) 
                        cycles-=4 
                        return 
                	case &hF0  'F2XM1
                        ST(0)=pow(2.0,ST(0))-1.0 
                        cycles-=200 
                        return 
                	case &hF1  'FYL2X
                        ST(1)=ST(1)*(Log_(ST(0))/Log_(2.0)) 
                        x87_pop() 
                        cycles-=250 
                        return 
                	case &hF2  'FPTAN
                        ST(0)=tan_(ST(0))
                        x87_push(1.0) 
                        npxs = npxs And inv(C2)
                        cycles-=235 
                        return 
                	case &hF3  'FPATAN
                        ST(1)=atan2_(ST(1),ST(0))
                        x87_pop() 
                        cycles-=250 
                        return 
                	case &hF6  'FDECSTP
                        TOP=(TOP-1) And 7 
                        return 
                	case &hF7  'FINCSTP
                        TOP=(TOP+1) And 7 
                        return 
                	case &hF8  'FPREM
                        temp64=CLngInt(ST(0)/ST(1))
                        ST(0)=ST(0) - ( ST(1)* CDbl(temp64) )
                        npxs = npxs And inv(C0 Or C1 Or C2 Or C3)
                        if (temp64 And 4) Then npxs = npxs Or C0 
                        if (temp64 And 2) Then npxs = npxs Or C3 
                        if (temp64 And 1) Then npxs = npxs Or C1 
                        cycles-=100 
                        return 
                	case &hFA  'FSQRT
                        ST(0)=sqrt(ST(0)) 
                        cycles-=83 
                        return 
                	case &hFB  'FSINCOS
                        td=ST(0) 
                        ST(0)=Sin_(td) 
                        x87_push(cos_(td)) 
                        npxs = npxs And inv( C2 )
                        cycles-=330 
                        return 
                	case &hFC  'FRNDINT
                        ST(0)=CDbl( x87_fround(ST(0)) ) ' en la 8.1 es "double"
                        cycles-=21 
                        return 
                	case &hFD  'FSCALE
                        temp64=CLngInt(ST(1)) 
                        ST(0)=ST(0)*pow(2.0,CDbl(temp64)) 
                        cycles-=30 
                        return 
                	case &hFE  'FSIN
                        ST(0)=Sin_(ST(0)) 
                        npxs = npxs And inv( C2 )
                        cycles-=300 
                        return 
                	case &hFF  'FCOS
                        ST(0)=cos_(ST(0)) 
                        npxs = npxs And inv( C2 )
                        cycles-=300 
                        return 
                End Select
        else
                Select Case As Const  (reg)
                	Case 0  'FLD single-precision
                        ts.i=geteal(): if abrt Then return 
                        x87_push(CDbl(ts.s)) 
                        cycles-=3 
                        return 
                	case 2  'FST single-precision
                        ts.s=CSng(ST(0)) 
                        seteal(ts.i) 
                        cycles-=7 
                        return 
                	case 3  'FSTP single-precision
                        ts.s=CSng(ST(0)) 
                        seteal(ts.i): if abrt Then return 
                        x87_pop() 
                        cycles-=7 
                        return 
                	case 4  'FLDENV
                        Select Case As Const  ((cr0 And 1) Or (op32 And &h100))
                        	Case &h000 , _ '16-bit real mode
                        	     &h001     '16-bit protected mode
                                npxc=readmemw_386(easeg,eaaddr) 
                                npxs=readmemw_386(easeg,eaaddr+2) 
                                tag =readmemw_386(easeg,eaaddr+4) 
                                TOP =(npxs Shr 11) And 7 
                        	case &h100 , _ '32-bit real mode
                        		  &h101 		'32-bit protected mode
                                npxc=readmemw_386(easeg,eaaddr) 
                                npxs=readmemw_386(easeg,eaaddr+4) 
                                tag =readmemw_386(easeg,eaaddr+8) 
                                TOP =(npxs Shr 11) And 7 
                        End Select
                        cycles-=IIf((cr0 And 1),34,44) 
                        return 
                	case 5  'FLDCW
                        tempw=geteaw() : if abrt Then return 
                        npxc=tempw 
                        cycles-=4 
                        return 
                	case 6  'FSTENV
                        Select Case As Const  ((cr0 And 1) Or (op32 And &h100))
                        	Case &h000  '16-bit real mode
                                writememw_386(easeg,eaaddr,npxc) 
                                writememw_386(easeg,eaaddr+2,npxs) 
                                writememw_386(easeg,eaaddr+4,tag) 
                                writememw_386(easeg,eaaddr+6,x87_pc_off) 
                                writememw_386(easeg,eaaddr+10,x87_op_off) 
                                exit Select 
                        	case &h001  '16-bit protected mode
                                writememw_386(easeg,eaaddr,npxc) 
                                writememw_386(easeg,eaaddr+2,npxs) 
                                writememw_386(easeg,eaaddr+4,tag) 
                                writememw_386(easeg,eaaddr+6,x87_pc_off) 
                                writememw_386(easeg,eaaddr+8,x87_pc_seg) 
                                writememw_386(easeg,eaaddr+10,x87_op_off) 
                                writememw_386(easeg,eaaddr+12,x87_op_seg) 
                                exit Select 
                        	case &h100  '32-bit real mode
                                writememw_386(easeg,eaaddr,npxc) 
                                writememw_386(easeg,eaaddr+4,npxs) 
                                writememw_386(easeg,eaaddr+8,tag) 
                                writememw_386(easeg,eaaddr+12,x87_pc_off) 
                                writememw_386(easeg,eaaddr+20,x87_op_off) 
                                writememl_386(easeg,eaaddr+24,(x87_op_off Shr 16) Shl 12) 
                                exit Select 
                        	case &h101  '32-bit protected mode
                                writememw_386(easeg,eaaddr,npxc) 
                                writememw_386(easeg,eaaddr+4,npxs) 
                                writememw_386(easeg,eaaddr+8,tag) 
                                writememl_386(easeg,eaaddr+12,x87_pc_off) 
                                writememl_386(easeg,eaaddr+16,x87_pc_seg) 
                                writememl_386(easeg,eaaddr+20,x87_op_off) 
                                writememl_386(easeg,eaaddr+24,x87_op_seg) 
                                exit Select 
                        End Select
                        If abrt Then Return
                        cycles-=IIf((cr0 And 1),56,67 )
                        return 
                	case 7  'FSTCW
                        seteaw(npxc):If abrt Then Return
                        cycles-=3 
                        return 
                End Select
        EndIf
        x86illegal() 
End Sub

Sub x87_da() 
        Dim As long templ 
     'Print #5,"DA:";modo,reg,Hex(rmdat32 And &hFF,2)
        if (modo=3) Then 
                Select Case As Const (rmdat32 And &hFF)
                	Case &hE9  'FUCOMPP
                        npxs = npxs And inv(C0 Or C2 Or C3)
                        if ST(0)=ST(1) Then 
                        	npxs = npxs Or C3 
                        ElseIf ST(0)<ST(1) Then
									npxs = npxs Or C0 
                        EndIf
                        x87_pop() 
                        x87_pop() 
                        cycles-=5 
                        return 
                End Select
        else
                templ=geteal(): if abrt Then return
                Select Case As Const  (reg)
                	Case 0  'FIADD
                        ST(0)=ST(0)+CDbl(templ) 
                        cycles-=20 
                        return 
                	case 1  'FIMUL
                        ST(0)=ST(0)*CDbl(templ) 
                        cycles-=22 
                        return 
                	case 2  'FICOM
                        npxs = npxs And inv(C0 Or C2 Or C3)
                        if ST(0)=CDbl(templ) Then 
                           npxs = npxs Or C3 
                        ElseIf ST(0)<CDbl(templ) Then
									npxs = npxs Or C0
                        EndIf
                        cycles-=15 
                        return 
                	case 3  'FICOMP
                        npxs = npxs And inv(C0 Or C2 Or C3)
                        if ST(0)=CDbl(templ) Then 
                           npxs = npxs Or C3 
                        ElseIf ST(0)<CDbl(templ) Then
									npxs = npxs Or C0
                        EndIf
                        x87_pop() 
                        cycles-=15 
                        return 
                	case 4  'FISUB
                        ST(0)=ST(0) - CDbl(templ) 
                        cycles-=20 
                        return 
                	case 5  'FISUBR
                        ST(0)=CDbl(templ) - ST(0) 
                        cycles-=19 
                        return 
                	case 6  'FIDIV
                        'ST(0)=ST(0) / CDbl(templ)
                        If x87_div(ST(0),ST(0),CDbl(templ)) Then return ' version 9
                        cycles-=84 
                        return 
                	case 7  'FIDIVR
                        'ST(0)=CDbl(templ) / ST(0)
                        If x87_div(ST(0),CDbl(templ),ST(0)) Then Return ' version 9
                        cycles-=84 
                        return 
                End Select
        EndIf
        x86illegal() 
End Sub

Sub x87_db() 
        Dim As Double t 
        Dim As Long templ
        Dim As LongInt temp64 ' este es nuevo en la 8.1

     'Print #5,"DB:";modo,reg,Hex(rmdat32 And &hFF,2)
        if (modo=3) Then 
                Select Case As Const (reg)
                	Case 4 
                        Select Case As Const (rmdat32 And &hFF)
                        	case &hE1 
                                return 
                        	case &hE2  'FCLEX
                                npxs = npxs And &hFF00 
                                return 
                        	case &hE3  'FINIT
                                npxc=&h37F 
                                npxs=0 
                                tag=&hFFFF 
                                TOP=0 
                                cycles-=17 
                                return 
                        	case &hE4 
                                return 
                        End Select
                       exit Select 
                End Select
        else
                Select Case As Const (reg)
                	Case 0  'FILD short
                        templ=geteal(): if abrt Then return 
                        x87_push( CDbl(templ) ) 
                        cycles-=9 
                        return 
                	case 2  'FIST Short
                		   temp64=x87_fround(ST(0)) ' nuevo en la 8.1
                        seteal( CLng(temp64) ) 
                        cycles-=28 
                        return 
                	case 3  'FISTP Short
                		   temp64=x87_fround(ST(0)) ' nuevo en la 8.1
                        seteal( CLng(temp64) ): if abrt Then return
                        x87_pop() 
                        cycles-=28 
                        return 
                	case 5  'FLD extended
                        t=x87_ld80(): if abrt Then return 
                        x87_push(t) 
                        cycles-=6 
                        return 
                	case 7  'FSTP extended
                        x87_st80(ST(0)): if abrt Then return 
                        x87_pop() 
                        cycles-=6 
                        return 
                End Select
        EndIf
        x86illegal() 
End Sub

Sub x87_dc() 

	Union ts1 
		As Double d 
		As ULongInt i 
	End Union
	Dim t As ts1	
    'Print #5,"DC:";modo,reg,Hex(rmdat32 And &hFF,2)
        if (modo=3) Then 
                Select Case As Const (rmdat32 And &hFF)
                	Case &hC0, &hC1, &hC2, &hC3 ,&hC4, &hC5, &hC6, &hC7 'FADD
                        ST(rmdat32 And 7)=ST(rmdat32 And 7) + ST(0) 
                        cycles-=8 
                        return 
                	case &hC8, &hC9, &hCA, &hCB ,&hCC, &hCD, &hCE, &hCF 'FMUL
                        ST(rmdat32 And 7)=ST(rmdat32 And 7) * ST(0) 
                        cycles-=16 
                        return 
                	case &hE0, &hE1, &hE2, &hE3 ,&hE4, &hE5, &hE6, &hE7 'FSUBR
                        ST(rmdat32 And 7)=ST(0) - ST(rmdat32 And 7) 
                        cycles-=8 
                        return 
                	case &hE8, &hE9, &hEA, &hEB ,&hEC, &hED, &hEE, &hEF 'FSUB
                        ST(rmdat32 And 7)=ST(rmdat32 And 7) - ST(0) 
                        cycles-=8 
                        return 
                	case &hF0, &hF1, &hF2, &hF3 ,&hF4, &hF5, &hF6, &hF7 'FDIVR
                        'ST(rmdat32 And 7)=(ST(0) /ST(rmdat32 And 7))
                        If x87_div(ST(rmdat32 And 7),ST(0),ST(rmdat32 And 7)) Then Return ' version 9
                        cycles-=73 
                        return 
                	case &hF8, &hF9, &hFA, &hFB ,&hFC, &hFD, &hFE, &hFF 'FDIV
                        'ST(rmdat32 And 7)=(ST(rmdat32 And 7) /ST(0))
                        If x87_div(ST(rmdat32 And 7),ST(rmdat32 And 7),ST(0)) Then return ' version 9
                        cycles-=73 
                        return 
                End Select
        Else
                t.i = readmeml_386(easeg,eaaddr) 
                t.i = t.i Or (CULngInt(readmeml_386(easeg,eaaddr+4)) Shl 32)
                if abrt Then return 
                Select Case As Const (reg)
                	case 0  'FADD Double
                        ST(0)+=t.d 
                        cycles-=8 
                        return 
                	case 1  'FMUL Double
                        ST(0)*=t.d 
                        cycles-=14 
                        return 
                	case 2  'FCOM Double
                        npxs = npxs And inv(C0 Or C2 Or C3)
                        if ST(0)=t.d Then 
                           npxs = npxs Or C3 
                        ElseIf ST(0)<t.d Then
									npxs = npxs Or C0
                        EndIf
                        cycles-=4 
                        return 
                	case 3  'FCOMP Double
                        npxs = npxs And inv(C0 Or C2 Or C3)
                        if ST(0)=t.d Then 
                           npxs = npxs Or C3 
                        ElseIf ST(0)<t.d Then
									npxs = npxs Or C0
                        EndIf
                        cycles-=4 
                        x87_pop() 
                        return 
                	case 4  'FSUB Double
                        ST(0)=ST(0) - t.d 
                        cycles-=8 
                        return 
                	case 5  'FSUBR Double
                        ST(0)=t.d - ST(0) 
                        cycles-=8 
                        return 
                	case 6  'FDIV Double
                        'ST(0)=ST(0)/t.d
                        If x87_div(ST(0),ST(0),t.d) Then Return ' version 9
                        cycles-=73 
                        return 
                	case 7  'FDIVR Double
                        'ST(0)=t.d/ST(0)
                        If x87_div(ST(0),t.d,ST(0)) Then Return ' version 9
                        cycles-=73 
                        return 
                End Select
        EndIf
        x86illegal() 
End Sub

Sub x87_dd() 
	
	Union ts1 
		As Double d 
		As ULongint i 
	End Union
	Dim t As ts1	

    'Print #5,"DD:";modo,reg,Hex(rmdat32 And &hFF,2)
        
        Dim As integer temp 
        if (modo=3) Then 
                Select Case As Const  (reg)
                	Case 0  'FFREE
                        tag = tag Or (3 Shl (((TOP+rmdat32) And 7) Shl 1)) 
                        cycles-=3 
                        return 
                	case 2  'FST
                        ST(rmdat32 And 7)=ST(0) 
                        temp=(tag Shr ((TOP And 7) Shl 1)) And 3 
                        tag = tag And inv(3 Shl (((TOP+rmdat32) And 7) Shl 1))
                        tag = tag Or  (temp Shl (((TOP+rmdat32) And 7) Shl 1)) 
                        cycles-=3 
                        return 
                	case 3  'FSTP
                        ST(rmdat32 And 7)=ST(0) 
                        temp=(tag Shr ((TOP And 7) Shl 1)) And 3 
                        tag = tag And inv(3 Shl (((TOP+rmdat32) And 7) Shl 1))
                        tag = tag Or  (temp Shl (((TOP+rmdat32) And 7) Shl 1)) 
                        x87_pop() 
                        cycles-=3 
                        return 
                	case 4  'FUCOM
                        npxs = npxs And inv(C0 Or C2 Or C3)
                        if ST(0)=ST(rmdat32 And 7) Then 
                           npxs = npxs Or C3 
                        ElseIf ST(0)<ST(rmdat32 And 7) Then
									npxs = npxs Or C0
                        EndIf
                        cycles-=4 
                        return 
                	case 5  'FUCOMP
                        npxs = npxs And inv(C0 Or C2 Or C3)
                        if ST(0)=ST(rmdat32 And 7) Then 
                        	npxs = npxs Or C3 
                        ElseIf ST(0)<ST(rmdat32 And 7) Then
									npxs = npxs Or C0 
                        EndIf
                        x87_pop() 
                        cycles-=4 
                        return 
                End Select
        else
                Select Case As Const (reg)
                	Case 0  'FLD double-precision
                        t.i=readmeml_386(easeg,eaaddr) 
                        t.i = t.i Or (CULngInt(readmeml_386(easeg,eaaddr+4)) Shl 32): if abrt Then return 
                        x87_push(t.d) 
                        cycles-=3 
                        return 
                	case 2  'FST double-precision
                        t.d=ST(0) 
                        writememl_386(easeg,eaaddr,t.i And &hffffffff) 
                        writememl_386(easeg,eaaddr+4,(t.i Shr 32)) 
                        cycles-=8 
                        return 
                	case 3  'FSTP double-precision
                        t.d=ST(0) 
                        writememl_386(easeg,eaaddr,t.i And &hffffffff) 
                        writememl_386(easeg,eaaddr+4,(t.i Shr 32)): if abrt Then return 
                        x87_pop() 
                        cycles-=8 
                        return 
                	case 4  'FRSTOR
                        Select Case As Const  ((cr0 And 1) Or (op32 And &h100))
                        	case &h000, _  '16-bit real mode
                        		  &h001		'16-bit protected mode
                                npxc=readmemw_386(easeg,eaaddr) 
                                npxs=readmemw_386(easeg,eaaddr+2) 
                                tag =readmemw_386(easeg,eaaddr+4) 
                                TOP =(npxs Shr 11) And 7 
                                eaaddr+=14 
                                exit Select 
                        	case &h100, _  '32-bit real mode
                        		  &h101		'32-bit protected mode
                                npxc=readmemw_386(easeg,eaaddr) 
                                npxs=readmemw_386(easeg,eaaddr+4) 
                                tag =readmemw_386(easeg,eaaddr+8) 
                                TOP =(npxs Shr 11) And 7 
                                eaaddr+=28 
                                exit Select 
                        End Select
                        ST(0)=x87_ld80(): eaaddr+=10 
                        ST(1)=x87_ld80(): eaaddr+=10 
                        ST(2)=x87_ld80(): eaaddr+=10 
                        ST(3)=x87_ld80(): eaaddr+=10 
                        ST(4)=x87_ld80(): eaaddr+=10 
                        ST(5)=x87_ld80(): eaaddr+=10 
                        ST(6)=x87_ld80(): eaaddr+=10 
                        ST(7)=x87_ld80() 
                        cycles-=IIf((cr0 And 1),34,44) 
                        return 
                	case 6  'FSAVE
                			npxs = (npxs And inv(7 shl 11)) or (TOP shl 11) ' de la 9.1
                        Select Case As Const  ((cr0 And 1) Or (op32 And &h100))
                        	Case &h000  '16-bit real mode
                                writememw_386(easeg,eaaddr,npxc) 
                                writememw_386(easeg,eaaddr+2,npxs) 
                                writememw_386(easeg,eaaddr+4,tag) 
                                writememw_386(easeg,eaaddr+6,x87_pc_off) 
                                writememw_386(easeg,eaaddr+10,x87_op_off) 
                                eaaddr+=14 
                                x87_st80(ST(0)): eaaddr+=10 
                                x87_st80(ST(1)): eaaddr+=10 
                                x87_st80(ST(2)): eaaddr+=10 
                                x87_st80(ST(3)): eaaddr+=10 
                                x87_st80(ST(4)): eaaddr+=10 
                                x87_st80(ST(5)): eaaddr+=10 
                                x87_st80(ST(6)): eaaddr+=10 
                                x87_st80(ST(7)) 
                                exit Select 
                        	case &h001  '16-bit protected mode
                                writememw_386(easeg,eaaddr,npxc) 
                                writememw_386(easeg,eaaddr+2,npxs) 
                                writememw_386(easeg,eaaddr+4,tag) 
                                writememw_386(easeg,eaaddr+6,x87_pc_off) 
                                writememw_386(easeg,eaaddr+8,x87_pc_seg) 
                                writememw_386(easeg,eaaddr+10,x87_op_off) 
                                writememw_386(easeg,eaaddr+12,x87_op_seg) 
                                eaaddr+=14 
                                x87_st80(ST(0)): eaaddr+=10 
                                x87_st80(ST(1)): eaaddr+=10 
                                x87_st80(ST(2)): eaaddr+=10 
                                x87_st80(ST(3)): eaaddr+=10 
                                x87_st80(ST(4)): eaaddr+=10 
                                x87_st80(ST(5)): eaaddr+=10 
                                x87_st80(ST(6)): eaaddr+=10 
                                x87_st80(ST(7)) 
                                exit Select 
                        	case &h100  '32-bit real mode
                                writememw_386(easeg,eaaddr,npxc) 
                                writememw_386(easeg,eaaddr+4,npxs) 
                                writememw_386(easeg,eaaddr+8,tag) 
                                writememw_386(easeg,eaaddr+12,x87_pc_off) 
                                writememw_386(easeg,eaaddr+20,x87_op_off) 
                                writememl_386(easeg,eaaddr+24,(x87_op_off Shr 16) Shl 12) 
                                eaaddr+=28 
                                x87_st80(ST(0)): eaaddr+=10 
                                x87_st80(ST(1)): eaaddr+=10 
                                x87_st80(ST(2)): eaaddr+=10 
                                x87_st80(ST(3)): eaaddr+=10 
                                x87_st80(ST(4)): eaaddr+=10 
                                x87_st80(ST(5)): eaaddr+=10 
                                x87_st80(ST(6)): eaaddr+=10 
                                x87_st80(ST(7)) 
                                exit select 
                        	case &h101  '32-bit protected mode
                                writememw_386(easeg,eaaddr,npxc) 
                                writememw_386(easeg,eaaddr+4,npxs) 
                                writememw_386(easeg,eaaddr+8,tag) 
                                writememl_386(easeg,eaaddr+12,x87_pc_off) 
                                writememl_386(easeg,eaaddr+16,x87_pc_seg) 
                                writememl_386(easeg,eaaddr+20,x87_op_off) 
                                writememl_386(easeg,eaaddr+24,x87_op_seg) 
                                eaaddr+=28 
                                x87_st80(ST(0)): eaaddr+=10 
                                x87_st80(ST(1)): eaaddr+=10 
                                x87_st80(ST(2)): eaaddr+=10 
                                x87_st80(ST(3)): eaaddr+=10 
                                x87_st80(ST(4)): eaaddr+=10 
                                x87_st80(ST(5)): eaaddr+=10 
                                x87_st80(ST(6)): eaaddr+=10 
                                x87_st80(ST(7)) 
                                exit Select 
                        End Select
                        cycles-=IIf((cr0 And 1),56,67 )
                        return 
                	case 7  'FSTSW
                        seteaw((npxs And &hC7FF) Or (TOP Shl 11)) 
                        cycles-=3 
                        return 
                End Select
        EndIf
        x86illegal() 
End Sub

Sub x87_de() 
    'Print #5,"DE:";modo,reg,Hex(rmdat32 And &hFF,2)
        Dim As Long templ 
        if (modo=3) Then 
                Select Case As Const  (rmdat32 And &hFF)
                	case &hC0, &hC1, &hC2, &hC3 ,&hC4, &hC5, &hC6, &hC7 'FADDP
                        ST(rmdat32 And 7)=ST(rmdat32 And 7)+ST(0) ' version 8.1 'STD(TOP) 
                        x87_pop() 
                        cycles-=8 
                        return 
                	case &hC8, &hC9, &hCA, &hCB ,&hCC, &hCD, &hCE, &hCF 'FMULP
                        ST(rmdat32 And 7)=ST(rmdat32 And 7)*ST(0) ' version 8.1 'STD(TOP)
                        x87_pop() 
                        cycles-=16 
                        return 
                	Case &hD9  'FCOMPP
                        npxs = npxs And inv(C0 Or C2 Or C3)
                        
                        ' esto de la 9.1 no se si usarlo, es un lio, y ... tiene utilidad?
                        If (CULngInt(ST(0))=(1 Shl 63)) And (CULngInt(ST(1))=0) Then
                        	npxs = npxs Or C0 ' Nasty hack to fix 80387 detection
                        ElseIf (ST(0)=ST(1)) Then ' este sustituye al que sigue de la 7.0
                        
                        'If ST(0)=ST(1) Then ' este seria el viejo de la 7.0
                           npxs = npxs Or C3 
                        ElseIf ST(0)<ST(1) Then
									npxs = npxs Or C0
                        EndIf
                        x87_pop() 
                        x87_pop() 
                        cycles-=5 
                        return 
                	case &hE0, &hE1, &hE2, &hE3 ,&hE4, &hE5, &hE6, &hE7 'FSUBRP
                        ST(rmdat32 And 7)=ST(0)-ST(rmdat32 And 7) 
                        x87_pop() 
                        cycles-=8 
                        return 
                	case &hE8, &hE9, &hEA, &hEB ,&hEC, &hED, &hEE, &hEF 'FSUBP
                        ST(rmdat32 And 7)=ST(rmdat32 And 7)-ST(0) ' version 8.1 'STD(TOP)
                        x87_pop() 
                        cycles-=8 
                        return 
                	case &hF0, &hF1, &hF2, &hF3 ,&hF4, &hF5, &hF6, &hF7 'FDIVRP
                        'ST(rmdat32 And 7)=ST(0)/ST(rmdat32 And 7)
                        If x87_div(ST(rmdat32 And 7),ST(0),ST(rmdat32 And 7)) Then Return ' version 9
                        x87_pop() 
                        cycles-=73 
                        return 
                	case &hF8, &hF9, &hFA, &hFB ,&hFC, &hFD, &hFE, &hFF 'FDIVP
                        'ST(rmdat32 And 7)=ST(rmdat32 And 7)/ST(0) ' version 8.1 'STD(TOP)
                        If x87_div(ST(rmdat32 And 7),ST(rmdat32 And 7),ST(0)) Then Return ' version 9
                        x87_pop() 
                        cycles-=73 
                        return 
                End Select
        else
                templ=CLng(CShort(geteaw())): if abrt Then return 
                Select Case As Const  (reg)
                	Case 0  'FIADD
                        ST(0)=ST(0)+CDbl(templ) 
                        cycles-=20 
                        return 
                	case 1  'FIMUL
                        ST(0)=ST(0)*CDbl(templ) 
                        cycles-=22 
                        return 
                	case 2  'FICOM
                        npxs = npxs And inv(C0 Or C2 Or C3)
                        if ST(0)=CDbl(templ) Then 
                           npxs = npxs Or C3 
                        ElseIf ST(0)<CDbl(templ) Then
									npxs = npxs Or C0
                        EndIf
                        cycles-=15 
                        return 
                	case 3  'FICOMP
                        npxs = npxs And inv(C0 Or C2 Or C3)
                        if ST(0)=CDbl(templ) Then 
                           npxs = npxs Or C3 
                        ElseIf ST(0)<CDbl(templ) Then
									npxs = npxs Or C0
                        EndIf
                        x87_pop() 
                        cycles-=15 
                        return 
                	case 4  'FISUB
                        ST(0)=ST(0)-CDbl(templ) 
                        cycles-=20 
                        return 
                	case 5  'FISUBR
                        ST(0)=CDbl(templ)-ST(0) 
                        cycles-=19 
                        return 
                	case 6  'FIDIV
                        'ST(0)=ST(0) / CDbl(templ)
                        If x87_div(ST(0),ST(0),CDbl(templ)) Then return ' version 9
                        cycles-=84 
                        return 
                	case 7  'FIDIVR
                        'ST(0)=CDbl(templ) / ST(0)
                        If x87_div(ST(0),CDbl(templ),ST(0)) Then Return ' version 9
                        cycles-=84 
                        return 
                End Select
        EndIf
        x86illegal() 
End Sub

Sub x87_df() 
        Dim As integer c 
        Dim As LongInt temp64 
        Dim As Short temp16 
        Dim As Double tempd 
        Dim As ubyte tempc 
     'Print #5,"DF:";modo,reg,Hex(rmdat32 And &hFF,2)
        if (modo=3) Then 
                Select Case As Const (reg)
                	Case 4 
                        Select Case As Const (rmdat32 And &hFF)
                        	Case &hE0  'FSTSW AX
                                AX=npxs 
                                cycles-=3 
                                return 
                        End Select
                       exit select 
                End Select
        else
                Select Case As Const (reg)
                	Case 0  'FILD short
                        temp16=CShort(geteaw()): if abrt Then return 
                        x87_push(CDbl(temp16)) 
                        cycles-=13 
                        return 
                	case 2  'FIST word
                        temp64=x87_fround(ST(0)) 
                        seteaw(CShort((temp64)))
                        cycles-=29 
                        return 
                	case 3  'FISTP word
                        temp64=x87_fround(ST(0)) 
                        seteaw(CShort((temp64))): if abrt Then return 
                        x87_pop() 
                        cycles-=29 
                        return 
                	case 5  'FILD long
                        temp64 = geteal(): if abrt Then return 
                        temp64 = temp64 Or (CULngInt(readmeml_386(easeg,eaaddr+4)) Shl 32)
                        x87_push(CDbl(temp64)) 
                        cycles-=10 
                        return 
                	case 6  'FBSTP
                        tempd=ST(0) 
                        If tempd<0.0 Then tempd=-tempd
                        For  c=0 To 8
                                tempc =CUByte(floor(fmod(tempd,10.0)))
                                tempd-=floor(fmod(tempd,10.0))
                                tempd/=10.0 
                                tempc = tempc Or (CUByte((floor(fmod(tempd,10.0)))) Shl 4)
                                tempd-=floor(fmod(tempd,10.0))
                                tempd/=10.0 
                                writememb_386(easeg,eaaddr+c,tempc) 
                        Next
                        tempc=CUByte(floor(fmod(tempd,10.0))) 
                        if (ST(0)<0.0) Then tempc = tempc Or &h80 
                        writememb_386(easeg,eaaddr+9,tempc): if abrt Then return 
                        x87_pop() 
                        return 
                	case 7  'FISTP long
                        temp64=x87_fround(ST(0)) 
                        seteal(CLng(temp64)) ' en el orig, no lleva clng, pero creo que lo necestia
                        writememl_386(easeg,eaaddr+4,temp64 Shr 32): if abrt Then return 
                        x87_pop() 
                        cycles-=29 
                        return 
                End Select
        EndIf
        x86illegal() 
End Sub
