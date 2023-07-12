module fdp_c_utils
  !! Contains functions for coverting C-strings to Fortran strings and back again.
  !!
  !! There are three types of string we must consider:
  !!
  !! - C string, of type `c_ptr`. This is the type that must be passed to and from
  !!   C functions.
  !! - `c_char` array, of type `character(1, kind=c_char) :: str_name(str_len)`.
  !!   This serves as an intermediate data type between Fortran strings and C strings.
  !! - Fortran strings, of type `character(:, kind=c_char)`.
  !!
  !! Converting from a C string to a Fortran string simply requires the function
  !! `fdp_c2f_str`. Converting from a Fortran string to a C string requires a little
  !! more effort:
  !! 
  !! - Convert to `c_char` array using `fdp_f_str_to_c_char_array`
  !! - Convert this to `c_ptr` using `fdp_c_char_array_to_c_str`

  implicit none

  private
  public :: fdp_c2f_str
  public :: fdp_f_str_to_c_char_array
  public :: fdp_c_char_array_to_c_str

contains

  ! Getting from C to Fortran 
  ! =========================

  subroutine fdp_c_char_array_to_f_str(c_char_array, c_char_array_len, f_str_ptr)
    !! Utility subroutine to convert a C character array to F string
    use, intrinsic :: iso_c_binding, only: c_char
    integer, intent(in) :: c_char_array_len
    character(c_char_array_len, kind=c_char), intent(in), target :: c_char_array(1)
    character(:, kind=c_char), intent(out), pointer :: f_str_ptr

    if( c_char_array_len .gt. 0 ) then
      f_str_ptr => c_char_array(1)
    else
      f_str_ptr => NULL()
    end if
  end subroutine fdp_c_char_array_to_f_str


  function fdp_c2f_str(c_str) result(f_str)
    !! Convert C string to Fortran string
    use, intrinsic :: iso_c_binding, only: c_char, c_ptr, c_f_pointer, c_associated
    type(c_ptr), intent(in) :: c_str
    character(kind=c_char), pointer :: char_array(:)
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

    ! Convert from C pointer to F pointer, giving it the length of the C string
    call c_f_pointer(c_str, char_array, [c_strlen(c_str)])

    ! Convert character array to Fortran string
    call fdp_c_char_array_to_f_str(char_array, size(char_array), f_str)
  end function fdp_c2f_str


  ! Getting from Fortran to C 
  ! =========================

  function fdp_f_str_to_c_char_array(f_str) result(c_char_array)
    !! Convert Fortran string to `c_char` array. This is used as an intermediate type
    !! which is then converted to `c_ptr`. This function handles the addition of the
    !! sentinel character `\0`.
    use, intrinsic :: iso_c_binding, only: c_char, c_ptr, c_null_char
    character(*), intent(in) :: f_str
    character(1, kind=c_char) :: c_char_array(len_trim(f_str)+1)
    integer :: ii, f_len

    f_len = len_trim(f_str)
    do ii=1, f_len
      c_char_array(ii) = f_str(ii:ii)
    end do
    c_char_array(f_len+1) = c_null_char
  end function fdp_f_str_to_c_char_array

  function fdp_c_char_array_to_c_str(c_char_array) result (ptr)
    !! Convert `c_char` array to `c_ptr`. The user should always build a named `c_char`
    !! array before converting to `c_ptr`. If the user instead passes the `c_char` array
    !! as a temporary, it will go out of scope, and the resulting pointer will be
    !! undefined.
    use, intrinsic :: iso_c_binding, only: c_char, c_ptr, c_loc
    character(1, kind=c_char), intent(in), target :: c_char_array(*)
    type(c_ptr) :: ptr
    
    ptr = c_loc(c_char_array)

  end function fdp_c_char_array_to_c_str

end module fdp_c_utils
