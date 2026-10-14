       program readdcd
       implicit none
       integer              :: i,ii,j,k,natom, ifeature, kk
       integer(4) :: dcdinfo(20), ntitle, indx(100000)
       real :: xdist,ydist,zdist,dist
       real :: coord1(3,100000), distmat(100000)
       character(4)     :: header
       character(80)    :: title(10) ! feature(100000)
       real(8)   :: boxsize(6)
!==========
       open(10, file='allapo-allrun-ca.dcd',form='unformatted',status='old')
        open(11, file='allapo-allrun-dist.txt') ! form='unformatted') !status='unknown')

!--------
       read(10) header, dcdinfo(1:20)
       read(10) ntitle, (title(i),i=1,ntitle)
       read(10) natom
!--------
        kk=0
        do j=1,natom
         do k=j+1,natom
           kk=kk+1
           indx(kk)=(j*10000)+k
         enddo
        enddo
        write(11,12)indx(1:kk)
  12    format(100000(i9,2x))    
        do ii=0,50000 ! loop over timeframes starts
           ifeature=0 ! initializing feature for every frame
       read(10) boxsize(1:6)
       read(10) coord1(1,1:natom)
       read(10) coord1(2,1:natom)
       read(10) coord1(3,1:natom)
!
!
!
                do j=1,natom 
                 do k=j+1,natom 
                 ifeature=ifeature+1
                xdist=coord1(1,j) - coord1(1,k) 
                ydist=coord1(2,j) - coord1(2,k) 
                zdist=coord1(3,j) - coord1(3,k) 
                dist= sqrt((xdist*xdist)+(ydist*ydist)+(zdist*zdist))
                distmat(ifeature)= dist
!


                 enddo
                enddo
                 write(11,17)distmat(1:ifeature)
 
                write(*,*) 'loop over timeframe no. ',ii,' ends'
                enddo  ! loop over timeframes end

   15        format(10f8.3) 
   16        format(3f8.3) 
   17        format(100000(f4.1,2x)) 
 
!
       stop
       close(10)
       end program readdcd
