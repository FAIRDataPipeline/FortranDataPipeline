module fairdatapipeline
  !! Fortran interface for the FairDataPipeline API.
  !!
  !! This relies on the C API, provided alongside cppDataPipeline.
  !! https://github.com/FAIRDataPipeline/cppDataPipeline

  use, intrinsic :: iso_c_binding, only: c_char, c_int, c_loc, c_ptr, c_size_t
  use fdp_c_utils, only: fdp_c_str_buffer, fdp_c2f_str, fdp_f2c_str, fdp_null_term

  implicit none

  private
  public :: FdpDataPipeline
  public :: fdp_log
  public :: fdp_set_log_level
  public :: fdp_get_log_level
  public :: FDP_ERR_NONE
  public :: FDP_ERR_CONFIG_PARSE
  public :: FDP_ERR_REST_API_QUERY
  public :: FDP_ERR_JSON_PARSE
  public :: FDP_ERR_VALIDATION
  public :: FDP_ERR_SYNC
  public :: FDP_ERR_WRITE
  public :: FDP_ERR_TOML
  public :: FDP_ERR_OTHER
  public :: FDP_LOG_TRACE
  public :: FDP_LOG_DEBUG
  public :: FDP_LOG_INFO
  public :: FDP_LOG_WARN
  public :: FDP_LOG_ERROR
  public :: FDP_LOG_CRITICAL
  public :: FDP_LOG_OFF

  type, bind(c) :: CFdpDataPipeline
    type(c_ptr) :: ptr
  end type

  type, private :: FdpDataPipeline
    !! Type representing an interface to the pipeline

    type(CFdpDataPipeline) :: c_struct

    contains

      procedure :: init => fdp_init
      procedure :: link_read => fdp_link_read
      procedure :: link_write => fdp_link_write
      procedure :: finalise => fdp_finalise
  end type

  enum, bind(c)
    enumerator :: &
      FDP_ERR_NONE = 0, &
      FDP_ERR_CONFIG_PARSE = 1, &
      FDP_ERR_REST_API_QUERY = 2, &
      FDP_ERR_JSON_PARSE = 3, &
      FDP_ERR_VALIDATION = 4, &
      FDP_ERR_SYNC = 5, &
      FDP_ERR_WRITE = 6, &
      FDP_ERR_TOML = 7, &
      FDP_ERR_OTHER = 8
  end enum

  enum, bind(c)
    enumerator :: &
      FDP_LOG_TRACE = 0, &
      FDP_LOG_DEBUG = 1, &
      FDP_LOG_INFO = 2, &
      FDP_LOG_WARN = 3, &
      FDP_LOG_ERROR = 4, &
      FDP_LOG_CRITICAL = 5, &
      FDP_LOG_OFF = 6
  end enum

contains

  subroutine fdp_init(self, config_file_path, script_file_path, token, err)
    !! Initialise the FAIR Data Pipeline object.
    !!
    !! Should be called once before any calls to `fdp_link_read` or `fdp_link_write`.
    !! If called more than once, returns FDP_ERR_OTHER.

    class(FdpDataPipeline), intent(inout), target :: self
      !! Object representing a connection to the pipeline. This will be passed to
      !! subsequent `fdp_link_read` and `fdp_link_write` calls.

    character(*, kind=c_char), intent(in) :: config_file_path
      !! Path to the `config.yaml` file for this FDP run.

    character(*, kind=c_char), intent(in) :: script_file_path
      !! Path to the script which initiates this FDP run.

    character(*, kind=c_char), intent(in) :: token
      !! Token used to connect to the FDP registry.

    integer(kind=c_int), optional, intent(out) :: err
      !! Error code.

    integer(kind=c_int) :: err_

    interface
      function c_fdp_init(pipeline, config, script, token) bind(C, name="fdp_init")
        use, intrinsic :: iso_c_binding, only: c_ptr, c_int
        implicit none
        type(c_ptr), intent(in), value :: pipeline 
        type(c_ptr), intent(in), value :: config
        type(c_ptr), intent(in), value :: script
        type(c_ptr), intent(in), value :: token
        integer(kind=c_int) :: c_fdp_init
      end function c_fdp_init
    end interface

    err_ = c_fdp_init(c_loc(self%c_struct%ptr), &
      fdp_f2c_str(fdp_null_term(config_file_path)), &
      fdp_f2c_str(fdp_null_term(script_file_path)), &
      fdp_f2c_str(fdp_null_term(token)) &
      )
    if(present(err)) err = err_
  end subroutine fdp_init


  subroutine fdp_finalise(self, err)
    !! Finalise the pipeline.
    !! 
    !1 Must be called after a call to fdp_init.
    !!
    !! Record all data products and meta data to the registry. Update the code run
    !! with all appropriate meta data.

    class(FdpDataPipeline), intent(inout), target :: self
    !! Object representing a connection to the pipeline. After calling this function,
    !! it will become unusable.

    integer(kind=c_int), optional, intent(out) :: err
      !! Error code.

    integer(kind=c_int) :: err_

    interface
      function c_fdp_finalise(pipeline) bind(C, name="fdp_finalise")
        use, intrinsic :: iso_c_binding, only: c_ptr, c_int
        implicit none
        type(c_ptr), intent(in), value :: pipeline 
        integer(kind=c_int) :: c_fdp_finalise
      end function c_fdp_finalise
    end interface

    err_ = c_fdp_finalise(c_loc(self%c_struct%ptr))
    if(present(err)) err = err_
  end subroutine fdp_finalise


  function fdp_link_write(self, data_product, err) result(data_store_path)
    !! Set a path to a given data product while recording its metadata for
    !! the code run.
    !!
    !! Must be called after `fdp_init` and before `fdp_finalise`.

    class(FdpDataPipeline), intent(in) :: self
      !! Object representing a connection to the pipeline.

    character(*, kind=c_char), intent(in) :: data_product
      !! Name of the data product to link to.

    integer(kind=c_int), optional, intent(out) :: err
      !! Error code.

    character(:, kind=c_char), allocatable :: data_store_path
      !! Path to the assigned data store location.

    character(4096, kind=c_char) :: data_store_buffer
    type(c_ptr) :: data_store_ptr
    integer(kind=c_int) :: err_

    interface
      function c_fdp_link_write(pipeline, data_product, data_store_path, length) &
          bind(C, name="fdp_link_write")
        use, intrinsic :: iso_c_binding, only: c_ptr, c_int, c_size_t
        implicit none
        type(c_ptr), intent(in), value :: pipeline 
        type(c_ptr), intent(in), value :: data_product
        type(c_ptr), intent(in), value :: data_store_path
        integer(kind=c_size_t), intent(in), value :: length
        integer(kind=c_int) :: c_fdp_link_write
      end function c_fdp_link_write
    end interface

    data_store_buffer = fdp_c_str_buffer(len(data_store_buffer))
    data_store_ptr = fdp_f2c_str(data_store_buffer)

    err_ = c_fdp_link_write(self%c_struct%ptr, &
      fdp_f2c_str(fdp_null_term(data_product)), &
      data_store_ptr, &
      int(len(data_store_buffer), kind=c_size_t) &
      )
    if(present(err)) err = err_

    data_store_path = fdp_c2f_str(data_store_ptr)
  end function fdp_link_write


  function fdp_link_read(self, data_product, err) result(data_store_path)
    !! Set a path to a given data product while recording its metadata for
    !! the code run.
    !!
    !! Must be called after `fdp_init` and before `fdp_finalise`.
 
    class(FdpDataPipeline), intent(in) :: self
      !! Object representing a connection to the pipeline.

    character(*, kind=c_char), intent(in) :: data_product
      !! Name of the data product to link to.

    integer(kind=c_int), optional, intent(out) :: err
      !! Error code.

    character(:, kind=c_char), allocatable :: data_store_path
      !! Path to the assigned data store location.

    character(4096, kind=c_char) :: data_store_buffer
    type(c_ptr) :: data_store_ptr
    integer(kind=c_int) :: err_

    interface
      function c_fdp_link_read(pipeline, data_product, data_store_path, length) &
          bind(C, name="fdp_link_read")
        use, intrinsic :: iso_c_binding, only: c_ptr, c_int, c_size_t
        implicit none
        type(c_ptr), intent(in), value :: pipeline 
        type(c_ptr), intent(in), value :: data_product
        type(c_ptr), intent(in), value :: data_store_path
        integer(kind=c_size_t), intent(in), value :: length
        integer(kind=c_int) :: c_fdp_link_read
      end function c_fdp_link_read
    end interface

    data_store_buffer = fdp_c_str_buffer(len(data_store_buffer))
    data_store_ptr = fdp_f2c_str(data_store_buffer)

    err_ = c_fdp_link_read(self%c_struct%ptr, &
      fdp_f2c_str(fdp_null_term(data_product)), &
      data_store_ptr, &
      int(len(data_store_buffer), kind=c_size_t) &
      )
    if(present(err)) err = err_

    data_store_path = fdp_c2f_str(data_store_ptr)
  end function fdp_link_read

  subroutine fdp_set_log_level(log_level)
    !! Set the current log level. Must call `fdp_init` first.
    integer(kind=c_int), intent(in) :: log_level

    interface
      subroutine c_fdp_set_log_level(log_level) bind(C, name="fdp_set_log_level")
        use, intrinsic :: iso_c_binding, only: c_int
        implicit none
        integer(kind=c_int), intent(in), value :: log_level
      end subroutine c_fdp_set_log_level
    end interface

    call c_fdp_set_log_level(log_level)
  end subroutine fdp_set_log_level

  function fdp_get_log_level() result(log_level)
    !! Return the current log level. Must call `fdp_init` first.
    integer(kind=c_int) :: log_level

    interface
      function c_fdp_get_log_level() bind(C, name="fdp_get_log_level")
        use, intrinsic :: iso_c_binding, only: c_int
        implicit none
        integer(kind=c_int) :: c_fdp_get_log_level
      end function c_fdp_get_log_level
    end interface

    log_level = c_fdp_get_log_level()
  end function fdp_get_log_level

  subroutine fdp_log(log_level, message, err)
    !! Write a message to the log.
    !!
    !! This will be passed to the C++ logger,
    !! `FairDataPipeline::logger::get_logger()->level() << msg`, where `level` is one
    !! of `trace`, `debug`, `info`, `warn`, `error`, or `critical`.

    integer(kind=c_int), intent(in) :: log_level
      !! The type of log message to write, e.g. FDP_LOG_INFO, FDP_LOG_ERROR.

    character(*, kind=c_char), intent(in) :: message
      !! The message to be written to the log

    integer(kind=c_int), optional, intent(out) :: err
      !! Error code. 1 if logging unsuccessful, 0 otherwise

    integer(kind=c_int) :: err_

    interface
      function c_fdp_log(lvl, msg) bind(C, name="fdp_log")
        use, intrinsic :: iso_c_binding, only: c_int, c_ptr
        implicit none
        integer(kind=c_int), intent(in), value :: lvl
        type(c_ptr), intent(in), value :: msg
        integer(kind=c_int) :: c_fdp_log
      end function c_fdp_log
    end interface

    err_ = c_fdp_log(log_level, fdp_f2c_str(fdp_null_term(message)))
    if(present(err)) err = err_
  end subroutine fdp_log

end module fairdatapipeline
