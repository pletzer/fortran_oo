SUBROUTINE test_file_object

    ! Import file object
    USE file_object_mod, only: file, file_withseparator

    IMPLICIT NONE

    TYPE(file) :: file_1
    TYPE(file_withseparator) :: file_2
    
    ! Create new file - construct new "file" object
    file_1 = file("file_1.txt")

    ! Inspect object
    PRINT '(A5,X,A10,X,A5,I3)', 'Name:', file_1%filename, 'Unit:', file_1%fileunit

    ! Write some data
    CALL file_1%write('Pi:')
    CALL file_1%write(3.14159)
    CALL file_1%write(951413)

    ! This will trigger an error message as we didn't implement
    ! output for complex numbers
    CALL file_1%write(CMPLX(3.14159))

    ! Create new file with separator
    file_2 = file_withseparator("file_2.txt")
    PRINT '(A5,X,A10,X,A5,I3)', 'Name:', file_2%filename, 'Unit:', file_2%fileunit

    ! Object can be used in exactly the same way as original object
    CALL file_2%write('Pi:')
    CALL file_2%write(3.14159)
    CALL file_2%write(951413)

END SUBROUTINE test_file_object