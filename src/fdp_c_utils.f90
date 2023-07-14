module fdp_c_utils
  !! Contains functions for coverting C-strings to Fortran strings and back again.
  !!
  !! In order to pass Fortran strings to C, we first need to append a null terminator
  !! character '\0', and then pass a `c_ptr` to the first character. A null-terminated
  !! string can be created using the function `fdp_null_term`, and a pointer to
  !! the start of this can be created using `fdp_f2c_str`. Converting a C string back
  !! to Fortran simply requires the function `fdp_c2f_str`.
  !!
  !! When interfacing with C functions, it is recommended to pass pointers using the
  !! type statement `type(c_ptr), intent(in), value`.

  implicit none

  private
  public :: fdp_null_term
  public :: fdp_f2c_str
  public :: fdp_c2f_str
  public :: fdp_c_str_buffer

contains

  ! Getting from Fortran to C
  ! =========================

  function fdp_null_term(str) result(null_str)
    !! Create new Fortran string with a null terminator
    use, intrinsic :: iso_c_binding, only: c_char, c_null_char
    character(*), intent(in) :: str
    character(len_trim(str)+1, kind=c_char) :: null_str

    null_str = trim(str) // c_null_char
  end function fdp_null_term

  function fdp_f2c_str(str) result(ptr)
    !! Convert string to `c_ptr`.
    use, intrinsic :: iso_c_binding, only: c_char, c_ptr, c_loc
    character(*, kind=c_char), intent(in), target :: str
    type(c_ptr) :: ptr

    ptr = c_loc(str)
  end function fdp_f2c_str

  ! Getting from C to Fortran
  ! =========================

  function fdp_c2f_str(c_str) result(f_str)
    !! Convert C string to Fortran string
    use, intrinsic :: iso_c_binding, only: c_char, c_ptr, c_f_pointer, c_associated
    type(c_ptr), intent(in) :: c_str
    character(1, kind=c_char), pointer :: char_array(:)
    character(:, kind=c_char), pointer :: f_str

    ! Uses strlen function from C stdlib
    interface
      function c_strlen(string) bind(C, name='strlen')
        use, intrinsic :: iso_c_binding, only: c_ptr, c_size_t
        implicit none
        type(c_ptr), intent(in), value :: string
        integer(c_size_t) :: c_strlen
      end function c_strlen
    end interface

    if(c_associated(c_str)) then
      call c_f_pointer(c_str, char_array, [c_strlen(c_str)])
      call char_array_to_str(char_array, size(char_array), f_str)
    else
      f_str => NULL()
    end if

  contains

    subroutine char_array_to_str(char_array, char_array_len, str)
      use, intrinsic :: iso_c_binding, only: c_char
      integer, intent(in) :: char_array_len
      character(char_array_len, kind=c_char), intent(in), target :: char_array(1)
      character(:, kind=c_char), intent(out), pointer :: str

      if( char_array_len .gt. 0 ) then
        str => char_array(1)
      else
        str => NULL()
      end if
    end subroutine char_array_to_str

  end function fdp_c2f_str

  function fdp_c_str_buffer(length)
    !! Create a string filled with null characters. Via `fdp_f2c_str`, this can be
    !! passed to C functions that modify a `char*` arg.
    use, intrinsic :: iso_c_binding, only: c_char, c_null_char
    integer, intent(in) :: length
    character(length, kind=c_char) :: fdp_c_str_buffer

    fdp_c_str_buffer = c_null_char
  end function fdp_c_str_buffer

end module fdp_c_utils
