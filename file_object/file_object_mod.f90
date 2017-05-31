MODULE file_object_mod

    IMPLICIT NONE

    ! Define "file" object with properties and
    ! methods ("type-bound procedures"). Also
    ! includes a destructor ("final subroutine")
    TYPE :: file
        character(len=:), ALLOCATABLE :: filename
        integer :: fileunit
        logical :: fileopen
    CONTAINS
        PROCEDURE :: write => file_write
        FINAL :: file_close
    END TYPE file

    ! Overload the default constructor with a
    ! custom constructor "file_init"
    INTERFACE file
        PROCEDURE :: file_init
    END INTERFACE

    ! Specialise object to include a separator
    TYPE, EXTENDS(file) :: file_withseparator
        ! Use a constant for simplicity
        CHARACTER(len=5) :: separator = '+++++'
    CONTAINS
        PROCEDURE :: write => file_withseparator_write
        FINAL :: file_withseparator_close
    END TYPE file_withseparator

    INTERFACE file_withseparator
        PROCEDURE :: file_withseparator_init
    END INTERFACE

CONTAINS

    ! Constructor must be a function - needs to return "file" object
    FUNCTION file_init(name)
        IMPLICIT NONE
        CHARACTER(len=*), INTENT(in) :: name
        ! Use TYPE if constructor is limited to "datafile"
        ! Use CLASS if constructor is polymorphic, but must
        ! allocate object in this case
        TYPE(file) :: file_init
        
        ! Set filename
        ALLOCATE(file_init%filename, SOURCE=name)

        ! Create new file, or overwrite existing file
        OPEN(NEWUNIT=file_init%fileunit, FILE=name, ACCESS='SEQUENTIAL', &
        & FORM='FORMATTED', STATUS='REPLACE')

    END FUNCTION file_init

    FUNCTION file_withseparator_init(name)
        IMPLICIT NONE
        CHARACTER(len=*), INTENT(in) :: name
        TYPE(file_withseparator) :: file_withseparator_init
        ALLOCATE(file_withseparator_init%filename, SOURCE=name)
        OPEN(NEWUNIT=file_withseparator_init%fileunit, FILE=name, ACCESS='SEQUENTIAL', &
        & FORM='FORMATTED', STATUS='REPLACE')
    END FUNCTION file_withseparator_init

    ! Simple polymorphic output routine
    SUBROUTINE file_write(this, data)
        IMPLICIT NONE
        ! Must be polymorphic
        CLASS(file), INTENT(in) :: this
        ! "Unlimited" polymorphic argument
        CLASS(*), INTENT(in) :: data

        ! Distinguish different data types and set
        ! pointer "output" to polymorphic variable
        ! "data"
        SELECT TYPE (output => data)
        TYPE IS (REAL)
            WRITE(this%fileunit, '(E12.5)') output
        TYPE IS (INTEGER)
            WRITE(this%fileunit, '(I6)') output
        TYPE IS (CHARACTER(*))
            WRITE(this%fileunit, '(A)') output
        CLASS DEFAULT
            PRINT '(A)', 'file_write: unsupported type'
        END SELECT

    END SUBROUTINE file_write

    ! Additional version for "file_withseparator"
    SUBROUTINE file_withseparator_write(this, data)
        IMPLICIT NONE
        ! Must be polymorphic
        CLASS(file_withseparator), INTENT(in) :: this
        ! "Unlimited" polymorphic argument
        CLASS(*), INTENT(in) :: data

        ! Write separator and reuse subroutine from
        ! base class
        WRITE(this%fileunit,'(A5)') this%separator
        CALL file_write(this, data)
        
    END SUBROUTINE file_withseparator_write

    ! Deconstructor for cleaning up
    SUBROUTINE file_close(this)
        IMPLICIT NONE
        TYPE(file), INTENT(INOUT) :: this
        LOGICAL :: opened

        INQUIRE(unit=this%fileunit, OPENED=opened)
        IF (opened) THEN
            PRINT '(A,X,A)', 'file_close: closing file', this%filename
            CLOSE(this%fileunit)
        END IF
        IF (ALLOCATED(this%filename)) THEN
            PRINT '(A,X,A)', 'file_close: deallocating this%filename'
            DEALLOCATE(this%filename)
        END IF

    END SUBROUTINE file_close

    SUBROUTINE file_withseparator_close(this)
        IMPLICIT NONE
        TYPE(file_withseparator), INTENT(INOUT) :: this
        LOGICAL :: opened

        INQUIRE(unit=this%fileunit, OPENED=opened)
        IF (opened) THEN
            PRINT '(A,X,A)', 'file_withseparator_close: closing file', this%filename
            CLOSE(this%fileunit)
        END IF
        IF (ALLOCATED(this%filename)) THEN
            PRINT '(A,X,A)', 'file_with_separator_close: deallocating this%filename'
            DEALLOCATE(this%filename)
        END IF

    END SUBROUTINE file_withseparator_close

END MODULE file_object_mod
