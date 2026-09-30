! tests/test_support.f90
! Scratch directories for tests of routines that find their files through
! getdir (siodir.txt), so no test reads or writes the installed Seas data.
! Scratch root is tests\tmp\ (gitignored), relative to the repo root, which
! is where the tests run from. Windows (cmd) only, like the DLL's paths.
module test_support
  use sio_io, only: siodir_file
  implicit none
  private
  public :: scratch_dir, point_getdir_at, point_getdir_at_missing, delete_file

contains

  ! Fresh scratch directory tests\tmp\<name>\ with an empty Data\ inside
  ! (any previous one is deleted). If template is given (a repo fixture
  ! directory such as tests\fixtures\base), its contents are copied in.
  subroutine scratch_dir(name, dir, template)
    character(len=*), intent(in)           :: name
    character(len=*), intent(out)          :: dir
    character(len=*), intent(in), optional :: template
    character(len=200) :: base
    ! no trailing '\' inside quotes: "dir\" can read as an escaped quote
    base = 'tests\tmp\' // trim(name)
    dir = trim(base) // '\'
    call execute_command_line('if exist "' // trim(base) // '" rmdir /s /q "' // &
                              trim(base) // '"')
    call execute_command_line('mkdir "' // trim(base) // '\Data"')
    if (present(template)) call execute_command_line('xcopy /e /i /q /y "' // &
         trim(template) // '" "' // trim(base) // '" >nul')
  end subroutine scratch_dir

  ! Write <dir>siodir.txt naming dir (Seas format: path ending in '?') and
  ! point getdir at it
  subroutine point_getdir_at(dir)
    character(len=*), intent(in) :: dir
    integer :: u
    open(newunit=u, file=trim(dir) // 'siodir.txt', status='replace', form='formatted')
    write(u, '(a)') trim(dir) // '?'
    close(u)
    siodir_file = trim(dir) // 'siodir.txt'
  end subroutine point_getdir_at

  ! Point getdir at a siodir.txt that does not exist
  subroutine point_getdir_at_missing()
    siodir_file = 'tests\tmp\no_such_dir\siodir.txt'
  end subroutine point_getdir_at_missing

  ! Delete a file if it exists (e.g. one file of a copied fixture)
  subroutine delete_file(path)
    character(len=*), intent(in) :: path
    integer :: u, ios
    open(newunit=u, file=trim(path), status='old', iostat=ios)
    if (ios == 0) close(u, status='delete')
  end subroutine delete_file

end module test_support
