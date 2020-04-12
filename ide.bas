
'io_sethandler(&h01f0, &h0008, ide_read_pri, ide_read_pri_w, NULL, ide_write_pri, ide_write_pri_w, NULL) 
'io_sethandler(&h03f6, &h0001, ide_read_pri, NULL,           NULL, ide_write_pri, NULL,            NULL) 
'io_sethandler(&h0170, &h0008, ide_read_sec, ide_read_sec_w, NULL, ide_write_sec, ide_write_sec_w, NULL) 
'io_sethandler(&h0376, &h0001, ide_read_sec, NULL,           NULL, ide_write_sec, NULL,            NULL) 


Declare Sub writeide(ide_board As Integer , addr As UShort , valor As UByte )  
Declare Sub writeidew(ide_board As Integer , valor As UShort )  
Declare Function readide(ide_board As Integer , addr As UShort ) As uByte  
Declare Function readidew(ide_board As Integer ) As UShort  
Declare Sub callbackide(ide_board As Integer )  


static shared As Integer ideboard
static shared As Integer idecallback(0 To 1)'={0,0}
static shared As Integer idetimes=0
static shared As Integer times30=0
static shared As Integer toctimes=0

static shared As Integer sec_ide ' direccion del sector a leer en el HD
static shared As String sec_sa ' para leer dos bytes del HD
                     sec_sa="  "

'static shared as Integer cur_ide(2)

Const IDE_TIME =5

'Bits of atastat 
Const ERR_STAT =&h01
Const DRQ_STAT =&h08 /' Data request '/
Const DSC_STAT =&h10
Const SERVICE_STAT =&h10
Const READY_STAT =&h40
Const BUSY_STAT =&h80

' Bits of error
Const ABRT_ERR =&h04 /' Command aborted '/
Const MCR_ERR =&h08 /' Media change request '/

/' ATA Commands '/
Const WIN_SRST =&h08 /' ATAPI Device Reset '/
Const WIN_RECAL =&h10
Const WIN_RESTORE =&h10 ' WIN_RECAL
Const WIN_READ =&h20 /' 28-Bit Read '/
Const WIN_READ_NORETRY =&h21 /' 28-Bit Read - no retry'/
Const WIN_WRITE =&h30 /' 28-Bit Write '/
Const WIN_WRITE_NORETRY =&h31 /' 28-Bit Write '/
Const WIN_VERIFY =&h40 /' 28-Bit Verify '/
Const WIN_FORMAT =&h50
Const WIN_SEEK =&h70
Const WIN_DRIVE_DIAGNOSTICS =&h90 /' Execute Drive Diagnostics '/
Const WIN_SPECIFY =&h91 /' Initialize Drive Parameters '/
Const WIN_PACKETCMD =&hA0 /' Send a packet command. '/
Const WIN_PIDENTIFY =&hA1 /' Identify ATAPI device '/
Const WIN_SETIDLE1 =&hE3
Const WIN_IDENTIFY =&hEC /' Ask drive to identify itself '/

enum 
		 IDE_NONE  = 0,
       IDE_HDD
End Enum

Type _IDE
     As Integer Type0
     As UByte atastat
     As UByte Error0
     As Integer secount,sector,cylinder,head,drive,cylprecomp
     As UByte Command0
     As UByte fdisk
     As Integer Pos0
     As Integer packlen
     As Integer spt,hpc
     As Integer tracks
     As Integer packetstatus
     As Integer Reset0
     As String*256 hdfile ' nombre del fichero a cargar
     As UShort buffer(0 To &hFFFF)
     As Integer irqstat
     As Integer service
     As Integer lba
     As ulongint lba_addr
End Type
static shared ide As _IDE




Sub ide_irq_raise()'ide as _IDE ) 
	if ((ide.fdisk And 2)=0) Then 
		picint(1 Shl 14)'IIf(ide.board,(1 Shl 15),(1 Shl 14))) 
	EndIf
	ide.irqstat=1 
   ide.service=1 
End Sub

Sub ide_irq_lower()'ide as _IDE ) 
   picintc(1 Shl 14)'IIf(ide.board,(1 Shl 15),(1 Shl 14))) 
	ide.irqstat=0 
End Sub

Sub ide_irq_update()'ide as _IDE ) 
	if (ide.irqstat>0) And (((pic2.pend Or pic2.ins) And &h40)=0) And ((ide.fdisk And 2)=0) Then 
         picint(1 Shl 14)'IIf(ide.board,(1 Shl 15),(1 Shl 14))) 
	ElseIf ((pic2.pend Or pic2.ins) And &h40) Then
			picintc(1 Shl 14)'IIf(ide.board,(1 Shl 15),(1 Shl 14)))
	EndIf
End Sub


 ' añade espacios al final de una cadena, hasta llegar a la longitud solicitada
Sub ide_padstr(str0 As UShort ptr , src As String , len0 As Integer ) 
	Dim As Integer i = any, v1 = any,v2 = any ,x = Any
	x=0
	for i = 0 To len0-1 Step 2
		if (i<Len(src)) Then
			v1 = Asc(Mid(src,i+1,1)) ' si la long. de la cadena es menor a la indicada en LEN0, cogemos el caracter
		Else
			v1 = Asc(" ") ' si es mayor a LEN0, metemos un espacio=32
		EndIf
		if (i<Len(src)) Then
			v2 = Asc(Mid(src,i+2,1)) ' si la long. de la cadena es menor a la indicada en LEN0, cogemos el caracter
		Else
			v2 = Asc(" ") ' si es mayor a LEN0, metemos un espacio=32
		EndIf
		str0[x] = v2 Or (v1 Shl 8) ' los guarda en modo inverso (tipo big endian, mayor primero), y con un '0' (0+caracter)
		x+=1
	Next
End Sub

'Fill in ide.buffer with the output of the "IDENTIFY DEVICE" command
Sub ide_identify()'IDE as _IDE ) 
	Dim i As Integer = Any
	For i=0 To 511
	  ide.buffer(i)=0
	Next
	ide.buffer(1) = ide.tracks /' Cylinders '/
	ide.buffer(3) = ide.hpc  /' Heads '/
	ide.buffer(6) = ide.spt  /' Sectors '/
	ide_padstr(@ide.buffer(10), "", 20) /' Serial Number '/
	ide_padstr(@ide.buffer(23), "v1.0", 8) /' Firmware '/
	ide_padstr(@ide.buffer(27), "PCemHD", 40) /' Model '/
	ide.buffer(49) = (1 Shl 9) /' LBA supported '/
	ide.buffer(50) = &h4000 /' Capabilities '/
	ide.buffer(60) = (ide.spt * ide.hpc * ide.tracks)  And &hFFFF /' Total addressable sectors (LBA) '/
	ide.buffer(61) = (ide.spt * ide.hpc * ide.tracks)  Shr  16 
	'For i=0 To 51
	'	Print i,Hex(ide.buffer(i),4)
	'	Sleep
	'Next
End Sub

/'
 * Return the sector offset for the current register values
 '/
Function ide_get_sector() As ulongint 
        If (ide.lba) Then Return ide.lba_addr
        'Print #5,ide.cylinder,ide.head,ide.sector, _ 
         '  ((( ide.cylinder * ide.hpc) + ide.head) * ide.spt) + (ide.sector - 1) ' :Sleep 5000,1
        	'                        heads(16)            sectors(63)
        Return ((( ide.cylinder * ide.hpc) + ide.head) * ide.spt) + (ide.sector - 1)
End Function


/'*
 * Move to the next sector using CHS addressing
 '/
Sub ide_next_sector()'IDE as _IDE ) 
        if (ide.lba) Then 
	         ide.lba_addr+=1 
        Else
	        	ide.sector+=1
	        	if ide.sector = ide.spt + 1 Then 
	        		ide.sector = 1 
	        		ide.head+=1 
	        		If ide.head = ide.hpc Then 
	        			ide.head = 0 
	        			ide.cylinder+=1 
	        		EndIf
	        	EndIf
        EndIf
End Sub



Sub writeidew(ide_board As Integer , valor As UShort ) 
        'Dim As _IDE ide = ide_drives'(cur_ide(ide_board)) 
		  'valor=(valor Shr 8) Or (valor Shl 8) ' esto es para BIG_ENDIAN, pero por si acas, revisar
		  
        ide.buffer(ide.pos0 Shr 1) = valor 
        ide.pos0+=2 
        if ide.packetstatus=4 Then 
        	  If ide.pos0>=(ide.packlen+2) Then 
        			 ide.packetstatus=5 
                idecallback(ide_board)=6*IDE_TIME 
                ide_irq_lower() ' revisar esta IRQ
                Return 
        	  EndIf
        ElseIf ide.packetstatus=5 Then 
                return 
        ElseIf (ide.command0 = WIN_PACKETCMD) And (ide.pos0>=&hC) Then
					 ide.pos0=0
                ide.atastat = BUSY_STAT 
                ide.packetstatus=1 
                idecallback(ide_board)=6*IDE_TIME 
                callbackide(ide_board) 
        ElseIf ide.pos0>=512 Then 
        	       ide.pos0=0 
                ide.atastat = BUSY_STAT 
                idecallback(ide_board)=6*IDE_TIME 
        EndIf
End Sub

Sub writeide(ByVal ide_board As Integer , ByVal addr As UShort , valor As UByte ) 
     'Dim As _IDE ide = ide_drives'(cur_ide(ide_board)) 
     
     addr = addr Or &h80 ' esto es para que los puertos secundarios, se lean como primarios (0x176=0x1f6 si sumo 0x80)

		' anulo, siempre tendre un HD
     'if (ide.type0 = IDE_NONE)  And  (addr <> &h1f6) Then return 

     Select Case As Const  (addr)
     	Case &h1F0  /' Data '/
                writeidew(ide_board, valor  Or  (valor  Shl  8))
                return 
     	case &h1F1  /' Features '/
                ide.cylprecomp=valor 
                return 
     	case &h1F2  /' Sector count '/
                ide.secount=valor 
                return 
     	case &h1F3  /' Sector '/
                ide.sector=valor 
                ide.lba_addr=(ide.lba_addr And &hFFFFF00) Or valor 
                return 
     	case &h1F4  /' Cylinder low '/
                ide.cylinder=(ide.cylinder And &hFF00) Or valor 
                ide.lba_addr=(ide.lba_addr And &hFFF00FF) Or (valor Shl 8) 
                return 
     	case &h1F5  /' Cylinder high '/
                ide.cylinder=(ide.cylinder And &hFF) Or (valor Shl 8) 
                ide.lba_addr=(ide.lba_addr And &hF00FFFF) Or (valor Shl 16) 
                return 
     	Case &h1F6  /' Drive/Head '/
     		' revisar estos dos que siguen, creo que es para cmabiar de HD, pero yo solo tengo UNO
                'cur_ide(ide_board)=((valor Shr 4) And 1)+(ide_board Shl 1) 
                'ide = ide_drives'(cur_ide(ide_board)) 
                'Print ide_board:sleep
                ide.head=valor And &hF 
                ide.lba =valor And &h40 
                ide.lba_addr=(ide.lba_addr And &h0FFFFFF) Or ((valor And &hF) Shl 24) 
                'Print #5,"ide.lba_addr:";Hex(ide.lba_addr,8)
                ide_irq_update() 
                return 
     	Case &h1F7  /' Command register '/
       	       If (ide.type0 = IDE_NONE) Then return 
        	       ide_irq_lower() ' revisar esta IRQ
                ide.command0=valor 
                ide.error0=0 
                Select Case As Const  (valor)
                	Case WIN_SRST  /' ATAPI Device Reset '/
                        ide.atastat = READY_STAT
                        idecallback(ide_board)=100*IDE_TIME 
                        return 
                	Case WIN_RESTORE, WIN_SEEK 
                        ide.atastat = READY_STAT 
                        idecallback(ide_board)=100*IDE_TIME 
                        return 
                	Case WIN_READ ,WIN_READ_NORETRY 
                        ide.atastat = BUSY_STAT 
                        idecallback(ide_board)=200*IDE_TIME 
                        return 
                	Case WIN_WRITE ,WIN_WRITE_NORETRY                         
                        ide.atastat = DRQ_STAT  Or  DSC_STAT  Or  READY_STAT 
                        ide.pos0=0 
                        return 
                	Case WIN_VERIFY 
                        ide.atastat = BUSY_STAT 
                        idecallback(ide_board)=200*IDE_TIME 
                        return 
                	Case WIN_FORMAT 
                        ide.atastat = DRQ_STAT 
                        ide.pos0=0 
                        return 
                	Case WIN_SPECIFY  /' Initialize Drive Parameters '/
                        ide.atastat = BUSY_STAT 
                        idecallback(ide_board)=200*IDE_TIME 
                        return 
                	Case WIN_DRIVE_DIAGNOSTICS ,WIN_PIDENTIFY ,WIN_SETIDLE1  /' Idle '/
                        ide.atastat = BUSY_STAT 
                        idecallback(ide_board)=200*IDE_TIME 
                        return 
                	Case WIN_IDENTIFY ,&hEF  /' Identify Device '/
                        ide.atastat = BUSY_STAT 
                        idecallback(ide_board)=200*IDE_TIME 
                        return 
                	Case WIN_PACKETCMD  /' ATAPI Packet '/
                		Print #5,"mas cdrom a eliminar":sleep
                        ide.packetstatus=0 
                        ide.atastat = BUSY_STAT 
                        idecallback(ide_board)=1 '30*IDE_TIME;
                        ide.pos0=0 
                        return 
                	Case &hF0 ' esta parece estar solita !!!!
                	Case Else
                		Print "error ide!!!"
                		ide.atastat = READY_STAT  Or  ERR_STAT  Or  DSC_STAT 
                		ide.error0 = ABRT_ERR 
                     ide_irq_raise() ' revisar IRQ
                     Return 
                End Select
                
     	Case &h3F6  /' Device control '/
                if ((ide.fdisk And 4)>0) And ((valor And 4)=0) And (ide.type0 <> IDE_NONE) Then 
                		idecallback(ide_board)=500*IDE_TIME 
                     ide.reset0  = 1 
                     ide.atastat = BUSY_STAT 
                EndIf
                ide.fdisk=valor 
                ide_irq_update() ' revisar esta IRQ
                return 
     End Select
     
End Sub


Function readide(ByVal ide_board As Integer , ByVal addr As UShort ) As UByte 
        'Dim As _IDE ide = ide_drives'(cur_ide(ide_board))

        Dim As UByte  temp 
        Dim As UShort tempw 
        
        'temp=0
        
        addr = addr Or &h80 ' esto es para que los puertos secundarios, se lean como primarios (0x176=0x1f6 si sumo 0x80)
        
        'if (ide.type0 = IDE_NONE) And (addr <> &h1f6) Then return &hff 
       
        Select Case As Const  (addr)
        
        	Case &h1F0  /' Data '/
                tempw = readidew(ide_board)
                temp = tempw  And &hff 
                 
        	case &h1F1  /' Error '/
                temp = ide.error0 
                 
        	case &h1F2  /' Sector count '/
                temp = CUByte(ide.secount) 
                 
        	case &h1F3  /' Sector '/
                temp = CUByte(ide.sector) 
                 
        	case &h1F4  /' Cylinder low '/
                temp = CUByte(ide.cylinder And &hFF) 
                 
        	case &h1F5  /' Cylinder high '/
                temp = CUByte(ide.cylinder Shr 8) 
                 
        	case &h1F6  /' Drive/Head '/
        ' me salto esto de curide, por que solo tengo un hd, siempre es 1
                'temp = CUByte(ide.head Or iif((cur_ide(ide_board) And 1) , &h10 , 0) Or iif(ide.lba , &h40 , 0) Or &ha0) 
                temp = CUByte(ide.head Or iif(ide.lba , &h40 , 0) Or &ha0) 
                 
        	case &h1F7  /' Status '/
                'if (ide.type0 = IDE_NONE) Then
                '		temp = 0 
                '     Exit Select 
                'EndIf
                ide_irq_lower()  ' revisar IRQ
                if (ide.fdisk And 4) Then 
                   temp = &h80 
                Else
                   temp = ide.atastat
                EndIf

        	case &h3F6  /' Alternate Status '/
                'if (ide.type0 = IDE_NONE) Then 
                '		 temp = 0 
                '      Exit Select
                'EndIf
                temp = ide.atastat

        End Select
        
        return temp 
End Function

Function readidew(ide_board As Integer ) As UShort 
        'Dim As _IDE ide = ide_drives'(cur_ide(ide_board))
        Dim As UShort temp = any
        
        temp = ide.buffer(ide.pos0 Shr 1)
        
        ide.pos0+=2 
        if ((ide.pos0>=512) And (ide.command0 <> WIN_PACKETCMD))   Or   ((ide.command0 = WIN_PACKETCMD) And (ide.pos0>=ide.packlen)) Then
    		    ide.pos0=0 
             'if (ide.command0 = WIN_PACKETCMD) Then 
             '	Print #5, "READIDEW llama a CDROM!!!":sleep
                  'callreadcd(ide) ' revisar esta llamada, que no se que es aun, pero parece del CDROM (por lo de READ_CD)
             'Else
                  ide.atastat = READY_STAT Or DSC_STAT
                  ide.packetstatus=0 
                  If (ide.command0 = WIN_READ) Or (ide.command0 = WIN_READ_NORETRY) Then 
                          ide.secount-=1 
                          If (ide.secount) Then 
                                  ide_next_sector() 
                                  ide.atastat = BUSY_STAT 
                                  idecallback(ide_board)=6*IDE_TIME 
                          EndIf
                  EndIf
            'EndIf
        EndIf
        
        return temp 
End Function


Sub callbackide(ide_board As Integer ) 
        'Dim As _IDE ide = ide_drives'(cur_ide(ide_board))
        Dim As ulongint ideaddr = any
        Dim As Integer c = any
        'ext_ide = ide ' esta no parece que se use

        if (ide.command0=&h30) Then times30+=1 
        
        if (ide.reset0) Then 
        			ide.atastat = READY_STAT Or DSC_STAT 
               ide.error0=1 
               ide.secount=1 
               ide.sector=1 
               ide.head=0 
               ide.cylinder=0 
               ide.reset0 = 0 
               Return 
        EndIf
        
        Select Case As Const  (ide.command0)
        
        case WIN_SRST  /'ATAPI Device Reset '/
                ide.atastat = READY_STAT Or DSC_STAT
                ide.error0 = 1 /'Device passed'/
                ide.secount = 1
                ide.sector = 1 
                'if (IDE_DRIVE_IS_CDROM(ide)) Then 
                '	ide.cylinder = &heb14 
                '  ide.atastat = 0 
                'else 
                  ide.cylinder = 0 ' solo dejo el HDD, no necesito CDROM
                'EndIf
                ide_irq_raise() ' revisar IRQ 
                'if (IDE_DRIVE_IS_CDROM(ide)) Then ide.service = 0 
                return 
        case WIN_RESTORE, WIN_SEEK 
                'if (IDE_DRIVE_IS_CDROM(ide)) Then {goto abort_cmd 
                ide.atastat = READY_STAT Or DSC_STAT 
                ide_irq_raise() ' revisar IRQ 
                return 
        case WIN_READ ,WIN_READ_NORETRY 
                'if (IDE_DRIVE_IS_CDROM(ide)) Then {goto abort_cmd 
                ideaddr = ide_get_sector() * 512 
                
                '''''''''''''''''''''''''''''''''''''''''''''
                'Print #5,"Leemos del fichero!!..... sin hacer":sleep
                	'fseeko64(ide.hdfile, ideaddr, SEEK_SET) 
                	'Locate 1,1:Print Hex(ideaddr,8):sleep
                	Seek 20,ideaddr+1
                	For sec_ide=0 To 255 ' como empleo una variable SHORT (2 bytes) solo leo 256 (que dan 512)
                		Get #20,,sec_sa
                		'sa=Mid(sa,2,1)+Mid(sa,1,1)
                		ide.buffer(sec_ide)=CvShort(sec_sa)
                		'Print Hex(Asc(Mid(sa,1,1)),2),Hex(Asc(Mid(sa,2,1)),2),Hex(CvShort(sa),4):Sleep
                	Next
                	'fread(ide.buffer, 512, 1, ide.hdfile) 
                '''''''''''''''''''''''''''''''''''''''''''''
                
                ide.pos0=0 
                ide.atastat = DRQ_STAT Or READY_STAT Or DSC_STAT 
                ide_irq_raise() ' revisar IRQ 
                readflash=1 
                return 
        case WIN_WRITE ,WIN_WRITE_NORETRY 
                'if (IDE_DRIVE_IS_CDROM(ide)) Then goto abort_cmd 
                ideaddr = ide_get_sector() * 512
                '''''''''''''''''''''''''''''''''''''''''''''
                'Print #5,"Grabamos al fichero!!"
                	'fseeko64(ide.hdfile, ideaddr, SEEK_SET) 
                	Seek 20,ideaddr+1
                	For sec_ide=0 To 255 ' como empleo una variable SHORT (2 bytes) solo guardo 256 (que dan 512)
                		sec_sa=MkShort(ide.buffer(sec_ide))
                		If IDE_GRABA Then Put #20,,sec_sa
                	Next
                	'fread(ide.buffer, 512, 1, ide.hdfile) 
                '''''''''''''''''''''''''''''''''''''''''''''
                
                ide_irq_raise() ' revisar IRQ 
                ide.secount-=1 
                if (ide.secount) Then 
                	  ide.atastat = DRQ_STAT Or READY_STAT Or DSC_STAT 
                    ide.pos0=0 
                    ide_next_sector() 
                else
                   ide.atastat = READY_STAT  Or  DSC_STAT 
                EndIf
                readflash=1 
                return 
        case WIN_VERIFY 
                'if (IDE_DRIVE_IS_CDROM(ide)) Then {goto abort_cmd 
                ide.pos0=0 
                ide.atastat = READY_STAT  Or  DSC_STAT 
                ide_irq_raise() ' revisar IRQ 
                readflash=1 
                return 
        case WIN_FORMAT 
                'if (IDE_DRIVE_IS_CDROM(ide)) Then {goto abort_cmd 
                ideaddr = ide_get_sector() * 512 
                
                ''''''''''''''''''''''''''''''''''''
                Print #5,"WIN_FORMAT sin hacer!!!":Sleep
                'fseeko64(ide.hdfile, ideaddr, SEEK_SET) 
                'memset(ide.buffer, 0, 512) 
                for c=0 To ide.secount-1
                      'fwrite(ide.buffer, 512, 1, ide.hdfile)
                Next
                ''''''''''''''''''''''''''''''''''''
                
                ide.atastat = READY_STAT  Or  DSC_STAT 
                ide_irq_raise() ' revisar IRQ 
                readflash=1 
                return 
        case WIN_DRIVE_DIAGNOSTICS 
                ide.error0=1 /'No error detected'/
                ide.atastat = READY_STAT  Or  DSC_STAT 
                ide_irq_raise() ' revisar IRQ 
                return 
        case WIN_SPECIFY  /' Initialize Drive Parameters '/
                'if (IDE_DRIVE_IS_CDROM(ide)) Then goto abort_cmd 
                
                ' OJO: esto coge parametros de la BIOS (me imagino), pero lo hace mal y coge ceros, y falla el HD
                ' por eso, los anulo por ahora, para que use los que yo pongo al cargar el HD
	                'ide.spt=ide.secount 
	                'ide.hpc=ide.head+1 
	                ide.secount=63'ide.spt
	                ide.head=16+1'ide.hpc+1 
	                                
                ide.atastat = READY_STAT  Or  DSC_STAT 
                ide_irq_raise() ' revisar IRQ 
                return 
        case WIN_PIDENTIFY  /' Identify Packet Device '/
                'if (IDE_DRIVE_IS_CDROM(ide)) Then {''                        pclog("ATAPI identify\n");ide_atapi_identify(ide) 
                '        ide.pos0=0 
                '        ide.error0=0 
                '        ide.atastat = DRQ_STAT  Or  READY_STAT  Or  DSC_STAT 
                '        ide_irq_raise() ' revisar IRQ 
                '        return 
                '}
                goto abort_cmd 
        case WIN_SETIDLE1 ,&hEF  /' Idle '/ 
                goto abort_cmd 
        case WIN_IDENTIFY  /' Identify Device '/
                'if (IDE_DRIVE_IS_CDROM(ide)) Then {ide.secount=1 
                '        ide.sector=1 
                '        ide.cylinder=&hEB14 
                '        ide.drive=ide.head=0 
                '        goto abort_cmd 
                '}
                if (ide.type0 <> IDE_NONE) Then 
                	ide_identify() 
                  ide.pos0=0 
                  ide.atastat = DRQ_STAT  Or  READY_STAT  Or  DSC_STAT 
                  ide_irq_raise() ' revisar IRQ 
                EndIf
                return 
        case WIN_PACKETCMD  /' ATAPI Packet '/
            Print "CDRAM WIN_PACKETCMD del CDROM , se usa ???":sleep
                'If ( IDE_DRIVE_IS_CDROM(ide)=0) Then goto abort_cmd 
                'if ( ide.packetstatus=0) Then 
                '	      ide.pos0=0 
                '        ide.secount = CUByte((ide.secount And &hF8) Or 1) 
                '        ide.atastat = DRQ_STAT  Or (ide.atastat And ERR_STAT) 
                'ElseIf (ide.packetstatus=1) Then 
                '	      ide.atastat = BUSY_STAT Or (ide.atastat And ERR_STAT) 
                '        atapicommand(ide_board) ' CDROM????
                'ElseIf (ide.packetstatus=2) Then 
                '	      ide.atastat = READY_STAT 
                '        ide.secount=3 
                '        ide_irq_raise() ' revisar IRQ 
                'ElseIf (ide.packetstatus=3) Then 
                '			ide.atastat = DRQ_STAT Or (ide.atastat And ERR_STAT) 
                '        ide_irq_raise() ' revisar IRQ 
                '        ide.packetstatus=&hFF 
                'ElseIf (ide.packetstatus=4) Then 
                '			ide.atastat = DRQ_STAT Or (ide.atastat And ERR_STAT) 
                '        ide_irq_raise() ' revisar IRQ 
                '        ide.pos0=2 
                'ElseIf (ide.packetstatus=5) Then 
                '        atapicommand(ide_board) ' CDROM???
                'ElseIf (ide.packetstatus=6) Then /'READ CD callback'/
					 '			ide.atastat = DRQ_STAT Or (ide.atastat And ERR_STAT)
                '        ide_irq_raise() ' revisar IRQ 
                'elseif (ide.packetstatus=&h80) Then /'Error callback'/
                '			ide.atastat = READY_STAT  Or  ERR_STAT 
                '        ide_irq_raise() ' revisar IRQ 
                'EndIf
                'return 
        End Select        
' GOTOS de rutinas de arriba
abort_cmd: 
	ide.atastat = READY_STAT  Or  ERR_STAT  Or  DSC_STAT 
	ide.error0 = ABRT_ERR 
	ide_irq_raise() ' revisar IRQ 
End Sub





' primario
Sub ide_write_pri(addr As UShort , valor As UByte ) 
        writeide(0, addr, valor) 
End Sub
Sub ide_write_pri_w(addr As UShort , valor As UShort ) 
        writeidew(0, valor) 
End Sub
Function ide_read_pri(addr As UShort ) As UByte 
        return readide(0, addr) 
End Function
Function ide_read_pri_w(addr As UShort ) As UShort 
        return readidew(0) 
End Function


' secundario
Sub ide_write_sec(addr As UShort , valor As UByte ) 
        writeide(1, addr, valor) 
End Sub
Sub ide_write_sec_w(addr As UShort , valor As UShort ) 
        writeidew(1, valor) 
End Sub
Function ide_read_sec(addr As UShort ) As UByte 
        return readide(1, addr) 
End Function
Function ide_read_sec_w(addr As UShort ) As UShort 
        return readidew(1) 
End Function





Sub loadhd( fichero As String, hdc_cilindros As integer, hdc_sectores As integer, hdc_cabezas As Integer) 
 
 		'Dim d As Integer
		'if (ide.hdfile = "") Then 
			ide.hdfile = fichero
			open fichero for binary as 20
			'Print Lof(20):sleep
		'Else
		'	Print "Error: fichero de DISCO DURO no encontrado":Sleep:End
		'EndIf
		
		' relojes IDE
		  idecallback(0)=0
		  idecallback(1)=0

		  'For d=0 To 3
		  	'  ide_drives(d).board = IIf((d And  2) , 1 , 0)
		  'Next
		  
		  'ide.board=1 ' no sirve para nada, solo para saber que IDE esta activo, si el 0 o el 1 (cada IDE tiene DOS HD)

        ide.spt = hdc_sectores ' 63 tipico
        ide.hpc = hdc_cabezas  ' 16 tipico
        ide.tracks = hdc_cilindros ' depende tamaño: 511 para 260 megas. SOLO la emplea la BIOS de autodetecccion de HD
        ide.type0 = IDE_HDD 
        
        'cur_ide(0) = 0
        'cur_ide(1) = 2
        
End Sub
