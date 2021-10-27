program Master_Mind_Filippos_Katsimalis
! This code simulates the classic board game "Master Mind". See: https://en.wikipedia.org/wiki/Mastermind_(board_game).
! Comments are in greeklish ¯\_(ツ)_/¯.
! Author: Filippos Katsimalis (fkatsimalis@gmail.com).


implicit none

integer, parameter:: number_of_colors = 6                 ! arithmos diathesimwn xrwmatwn
integer, parameter:: number_of_rows = 12                  ! megisto plhthos prospatheiwn
integer, parameter:: number_of_holes = 4                  ! arithmos epilogwn ana prospatheia
logical:: allow_duplicates                                ! an .TRUE. tote o kryfos kwdikos mporei na periexei to idio xrwma perissoteres apo mia fores (an htan 'parameter' de tha mporouse na epileksei o xrhsths to eidos kwdikou pou thelei)
integer:: key(number_of_holes)                            ! o kryfos kwdikos pou kaleitai na apokalypsei o xrhsths
integer:: table(number_of_rows, number_of_holes + 2)      ! to tamplo tou paixnidiou
character(10):: epilogh, temp
integer:: st = 0, usertry, row, i, j, digit, shorter, k, r, w, high
real:: x


huge: do                     ! an o xrhsths thelei na ksanapeksei => cycle huge | an o xrhsths thelei na stamathsei => exit huge

call system('color 02')      ! prasina grammata se mavro fonto

print*
print*
print*,"Welcome to MasterMind! Press 'enter' to start."   ! mhnyma kaloswrismatos
read*

call system('cls')           ! katharismos othonhs

allow_duplicates = .false.   ! arxikopoihsh ws .false.

do
  call system('color 02')
  print*
  print*
  print*, "The target color combination is represented by a 4-digit code. Each digit is an integer from 1 to 6 (number of colors)."
  print*, "The player is the codebreaker and the computer is the codemaker. The player has 12 tries to guess the target code."
  print*, "A .txt file named 'Highscore' must exist in the same folder with the executable."
  print*, "Before the first game, it must contain 'Highscore: 12'."
  print*
  print*, "Feedback on the user's guess is given by the number of 'RED' and 'WHITE' (key pegs)."
  print*, "One 'RED' (key peg) is added for each correct code digit (color) from the guess which is also in the right position."
  print*, "One 'WHITE' (key peg) is added for each correct code digit (color) from the guess which is in the wrong position."
  print*
  print*, "Press 'F' for a code without duplicates, else press 'T'."         ! o xrhsths exei dynatothta epiloghs tou eidous tou kwdikou
  print*
  read (*,*, iostat = st) epilogh  
    if (st == 0 .and. (trim(epilogh) == 'F' .or. trim(epilogh) == 'f' .or. trim(epilogh) == 'T' .or. trim(epilogh) == 't')) then   ! elegxos egkyrwthtas apanthshs
      exit
    else
      print*, 'Invalid input. Please try again.'
      print*
      print*
    end if
end do
  
call system('cls')

if (epilogh == 'T' .or. epilogh == 't') allow_duplicates = .true.


call random_seed()


if (allow_duplicates) then
  do i = 1, number_of_holes
    call random_number(x)
    x = 1 + nint(x * (number_of_colors - 1))
    key(i) = int(x)                  ! allow_duplicates = .true. => de mas endiaferei to kathe pshfio na einai monadiko
  end do
else
  key = 0
  k = 0                              ! k => metrhths twn diaforetikwn pshfiwn
  do
    if (k == number_of_holes) exit   ! otan vrethoun 4 diaforetika pshfia => exit    
    call random_number(x)
    x = 1 + nint(x * (number_of_colors - 1))
    if (all(key /= int(x))) then     ! allow_duplicates = .false. => prepei o kathe tyxaios na einai monadikos
      k = k + 1                      ! vrethhke neo diaforetiko pshfio => afkshsh tou k kata 1
      key(k) = int(x)
    else
    end if
  end do
end if

    
open (10,file='Highscore.txt',status='old',iostat=st)   ! anoigma tou arxeiou gia apothhkeysh tou highscore me elegxo an to arxeio yparxei
if (st /= 0) then
  stop "Error: File 'Highscore.txt' does not exist."   
else
end if

rewind(10)                                              ! rewind wste se polaples ekteleseis tou programmatos na mh ftasei o deikths sto EOF
read(10,*) temp, high                                   ! diavazei to prohgoumeno highscore

out: do row = 1, number_of_rows                         ! oi epanalhpseis einai toses oses kai oi prospatheies tou xrhsth
   in: do 
         print*
         print*
         print 500, 'Try:', row, '. Enter your guess (without blanks or commas).'
         print*
         print*
         read (*,*, iostat = st) usertry                                  ! elegxos egkyrwthtas tou kwdikou tou xrhsth
         if (st == 0 .and. usertry >= 1111 .and. usertry <= 6666 ) then   ! xwris ton periorismo 1111<=usertry<=6666 o xrhsths tha mporouse na dwsei kwdikous me perissotera egkyra pshfia apo 4 (12345, 121212, 11111 ...) 
           shorter = usertry
           do j = number_of_holes, 1, -1                                  ! antistrofh epanalhpsh
             digit = mod (shorter, 10)
             if (digit < 1 .or. digit > number_of_colors) then            ! an kapoio pshfio einai >6 h <1 tote o kwdikos den einai egkyros => cycle in
               print*, 'Invalid input. The code must contain 4 integers from 1 to 6. Please try again.'
               print*
               print*
               cycle in
             else
             table(row, j) = digit                                        ! epeidh ta pshfia ta diavazei apo to telos pros thn arxh arxizei na ta apothhkeyei apo to telos tou pinaka (mesw tou deikth j ths antistrofhs epanalhpshs)
             shorter = shorter / 10
             end if
           end do
           exit in                                                        ! an o kwdikos einai egkyros => exit
         else
           print*, 'Invalid input. The code must contain exactly 4 digits. Please try again.'
           print*
           print*
         end if
       end do in
       
    
       call check_row(row)       ! subroutine pou elegxei ton kwdiko tou xrhsth kai enhmerwnei to tamplo (RED kai WHITE)   
       call display_table(row)   ! emfanizei to tamplo => dinei ston xrhsth 'feedback' sxetika me ton kwdiko tou (RED kai WHITE)

       if ((4 - r) <= 1e-6) then ! an RED = 4 shmainei oti o xrhsths vrhke ton kryfo kwdiko
         do i = 1, 3
           print *, char(7)      ! hxhtikh eidopoihsh 3 fores
         end do
         print*
         print*
         print 400, 'YOU WIN!!! You found the target code:', key(:)
         print*
         print*
         if (row < high) then              ! an o arithmos prospatheiwn einai mikroteros tou highscore tote: 
           rewind(10)
           write(10,300) row               ! enhmerwnetai to highscore
           print*, 'New Highscore:', row   ! enhmerwnetai o xrhsths oti ekane highscore
           print*
           print*
         else
         end if
         print*, 'Do you want to play again? (yes - no)'
         print*
         read*, epilogh
         if (trim(epilogh) == 'yes' .or. trim(epilogh) == 'YES' .or. trim(epilogh) == 'y' .or. trim(epilogh) == 'Y') then  
           call system('cls')
           cycle huge
         else
           exit huge
         end if
       else if ((number_of_rows - row) <= 1e-6) then   ! an o xrhsths eftase sth 12h prospatheia xwris na vrei ton kryfo kwdiko tote tou fanerwnetai o kryfos kwdikos
         print*
         print*
         print 400, 'You run out of tries. The target code is:', key(:)
         print*
         print*
         print*, 'Do you want to play again? (yes - no)'
         print*
         read*, epilogh
         if (trim(epilogh) == 'yes' .or. trim(epilogh) == 'YES' .or. trim(epilogh) == 'y' .or. trim(epilogh) == 'Y') then
           call system('cls')
           cycle huge
         else
           exit huge
         end if
       else  
       end if

    end do out

end do huge

close(10)   ! kleisimo tou arxeiou 'Highscore'

print*
print*
print*, "Press 'enter' to exit."
read*

300 format('Highscore:',1x,i0)
400 format(a,1x,4(i0,1x))
500 format(a,1x,i0,a)

contains

subroutine check_row(row)
  integer, intent(in):: row
  logical:: help1(number_of_holes), help2(number_of_holes)   ! 2 vohthhtikoi logikoi pinakes | help1 => an help1(j) = .true. tote key(j) = table(row, j) => RED gia to pshfio tou table kai tou key sth thesh j &
  integer:: q                                                !                              &| help2 => an help2(q) = .true. tote key(q) = table(row, j) => WHITE gia to pshfio tou key sth thesh q
  
 help1 = .false.                             ! arxikopoihsh tou help1 ws .false. => dhladh den yparxoun pshfia se swsth thesh (kanena RED)
       r = 0                                 ! r => o arithmos twn pshfiwn se swsth thesh (RED)
       do j = 1, number_of_holes             ! j => skanarei taytoxrona ta pshfia tou key kai tou table ths idias theshs | elegxos 4 fores (mia gia gia kathe pshfio) 
         if (table(row, j) == key(j)) then   ! an to pshfio tou kryfou kwdikou sth thesh j einai idio me ayto tou kwdikou tou xrhsth sth thesh j tote:
           help1(j) = .true.                 ! allagh ths katastashs tou help1 sth thesh j se .true. => dhladh ta pshfia tou key kai tou table sth thesh j einai idia
           r = r + 1                         ! aykshsh tou arithmou twn RED kata 1 
           else
           end if
       end do

help2 = .false.                              ! arxikopoihsh tou help2 ws .false. => dhladh den yparxoun idia pshfia se lathos thesh (kanena WHITE ston key)
       w = 0                                 ! w => o arithmos twn idiwn pshfiwn se lathos thesh (WHITE)
       do j = 1, number_of_holes             ! j => skanarei ta pshfia tou table | elegxos 4 fores (mia gia kathe pshfio tou table)
  small: do q = 1, number_of_holes           ! q => skanarei ta pshfia tou key | elegxos 4 fores (mia gia kathe pshfio tou key) gia kathe pshfio tou table 
           if ((.not. help1(j)) .and. (.not. help1(q)) .and. (.not. help2(q)) .and. table(row, j) == key(q)) then          ! gia na isxyei h synthhkh prepei:
             w = w + 1           ! aykshsh twn white kata 1                                                                !  1) help1(j) = help1(q) = .false. => ta pshfia tou table kai tou key stis theseis j kai q na mhn einai RED 
             help2(q) = .true.   ! allagh tou help2(q) se .true. => pleon to pshfio tou key sth thesh q einai WHITE        !  2) help2(q) = .false. => to pshfio tou key sth thesh q na mhn einai WHITE 
             exit small          ! eksodos giati de theloume na ginei nea sygkrish me to pshfio tou table sth thesh j      !  3) table(row, j) = key(q) => ta pshfia tou table kai tou key stis theseis j kai q na einai isa
           else
           end if
         end do small
       end do
   

     table(row, number_of_holes + 1) = r    ! enhmerwsh tou pinaka sxetika me ta RED sth prospatheia row
     table(row, number_of_holes + 2) = w    ! enhmerwsh tou pinaka sxetika me ta WHITE sth prospatheia row
    

end subroutine check_row


subroutine display_table(row)
  integer, intent(in):: row
  integer:: i

  call system('cls')
  call system('color 02')
 
  print* 
  print*, '#####################################################'
  print*, '########             MasterMind              ########' 
  print*, '#####################################################'
  print'(a,1x,i0,a)', '                   //Highscore:', high,'\\'   ! enhmerwsh tou xrhsth sxetika me to highscore
  print*
  
  do i = 1, row
    print 200, i, ')', table(i,1:4), '|', 'RED =', table(i, number_of_holes + 1), 'WHITE =', table(i, number_of_holes + 2)   ! emfanish tou tamplo sthn othonh me katallhlh morfopoihsh
  end do
  

200 format(1x,5('#'),2x,i2,a,2x,4(i2),3x,a,3x,a,1x,i0,2x,a,1x,i0,3x,5('#'))


end subroutine display_table


end program 