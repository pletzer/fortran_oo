PROGRAM file_object_driver
    ! Need to create file object in a subroutine, deconstructor
    ! is not called on END PROGRAM
    CALL test_file_object()
END PROGRAM file_object_driver