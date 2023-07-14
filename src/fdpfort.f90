module fdpfort
  !! Fortran interface for the FairDataPipeline API.
  !!
  !! This relies on the C API, provided alongside cppDataPipeline.
  !! https://github.com/FAIRDataPipeline/cppDataPipeline

  use, intrinsic :: iso_c_binding, only: c_ptr
  use fdp_c_utils

  implicit none

  private
  public :: FdpDataPipeline
  public :: fdp_init
  public :: fdp_finalise
  public :: fdp_link_read
  public :: fdp_link_write
  !public :: fdp_log
  !public :: fdp_set_log_level
  !public :: fdp_get_log_level
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

  type, bind(c) :: FdpDataPipeline
    !! Interface to the `FdpDataPipeline` C struct
    type(c_ptr) :: ptr
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

  function fdp_init(data_pipeline, config_file_path, script_file_path, token)
    !! Initialise the FAIR Data Pipeline object.
    !!
    !! Should be called once before any calls to `fdp_link_read` or `fdp_link_write`.
    !! If called more than once, returns FDP_ERR_OTHER.
    use, intrinsic :: iso_c_binding, only: c_char, c_int, c_loc

    type(FdpDataPipeline), intent(inout), target :: data_pipeline
      !! Object representing a connection to the pipeline. This will be passed to
      !! subsequent `fdp_link_read` and `fdp_link_write` calls.
    character(*, kind=c_char), intent(in) :: config_file_path
      !! Path to the `config.yaml` file for this FDP run.
    character(*, kind=c_char), intent(in) :: script_file_path
      !! Path to the script which initiates this FDP run.
    character(*, kind=c_char), intent(in) :: token
      !! Token used to connect to the FDP registry.
    integer(kind=c_int) :: fdp_init
      !! Error code.

    interface
      function c_fdp_init(pipeline, config, script, token) bind(C, name="fdp_init")
        use, intrinsic :: iso_c_binding, only: c_int, c_ptr
        implicit none
        type(c_ptr), intent(in), value :: pipeline 
        type(c_ptr), intent(in), value :: config
        type(c_ptr), intent(in), value :: script
        type(c_ptr), intent(in), value :: token
        integer(kind=c_int) :: c_fdp_init
      end function c_fdp_init
    end interface

    fdp_init = c_fdp_init(c_loc(data_pipeline%ptr), &
      fdp_f2c_str(fdp_null_term(config_file_path)), &
      fdp_f2c_str(fdp_null_term(script_file_path)), &
      fdp_f2c_str(fdp_null_term(token)) &
      )
  end function fdp_init

  function fdp_finalise(data_pipeline)
    !! Finalise the pipeline.
    !! 
    !1 Must be called after a call to fdp_init.
    !!
    !! Record all data products and meta data to the registry. Update the code run
    !! with all appropriate meta data.
    use, intrinsic :: iso_c_binding, only: c_int, c_loc

    type(FdpDataPipeline), intent(inout), target :: data_pipeline
      !! Object representing a connection to the pipeline. After calling this function,
      !! it will become unusable.
    integer(kind=c_int) :: fdp_finalise
      !! Error code.

    interface
      function c_fdp_finalise(data_pipeline) bind(C, name="fdp_finalise")
        use, intrinsic :: iso_c_binding, only: c_int, c_ptr
        implicit none
        type(c_ptr), intent(in), value :: data_pipeline 
        integer(kind=c_int) :: c_fdp_finalise
      end function c_fdp_finalise
    end interface

    fdp_finalise = c_fdp_finalise(c_loc(data_pipeline%ptr))
  end function fdp_finalise

  subroutine fdp_link_write(data_pipeline, data_product, data_store_path, err)
    !! Set a path to a given data product while recording its metadata for
    !! the code run.
    !!
    !! Must be called after `fdp_init` and before `fdp_finalise`.
 
    use, intrinsic :: iso_c_binding, only: c_char, c_int, c_size_t, c_ptr

    type(FdpDataPipeline), intent(in) :: data_pipeline
      !! Object representing a connection to the pipeline.
    character(*, kind=c_char), intent(in) :: data_product
      !! Name of the data product to link to.
    character(:, kind=c_char), allocatable, intent(out) :: data_store_path
      !! Path to the assigned data store location.
    integer(kind=c_int), intent(out) :: err
      !! Error code.

    character(4096, kind=c_char) :: data_store_buffer
    type(c_ptr) :: data_store_ptr

    interface
      function c_fdp_link_write(data_pipeline, data_product, data_store_path, length) &
          bind(C, name="fdp_link_write")
        use, intrinsic :: iso_c_binding, only: c_int, c_size_t, c_ptr
        implicit none
        type(c_ptr), intent(in), value :: data_pipeline 
        type(c_ptr), intent(in), value :: data_product
        type(c_ptr), intent(in), value :: data_store_path
        integer(kind=c_size_t), intent(in), value :: length
        integer(kind=c_int) :: c_fdp_link_write
      end function c_fdp_link_write
    end interface

    data_store_buffer = fdp_c_str_buffer(len(data_store_buffer))
    data_store_ptr = fdp_f2c_str(data_store_buffer)

    err = c_fdp_link_write(data_pipeline%ptr, &
      fdp_f2c_str(fdp_null_term(data_product)), &
      data_store_ptr, &
      int(len(data_store_buffer), kind=c_size_t) &
      )

    data_store_path = fdp_c2f_str(data_store_ptr)
  end subroutine fdp_link_write


  subroutine fdp_link_read(data_pipeline, data_product, data_store_path, err)
    !! Set a path to a given data product while recording its metadata for
    !! the code run.
    !!
    !! Must be called after `fdp_init` and before `fdp_finalise`.
 
    use, intrinsic :: iso_c_binding, only: c_char, c_int, c_size_t, c_ptr

    type(FdpDataPipeline), intent(in) :: data_pipeline
      !! Object representing a connection to the pipeline.
    character(*, kind=c_char), intent(in) :: data_product
      !! Name of the data product to link to.
    character(:, kind=c_char), allocatable, intent(out) :: data_store_path
      !! Path to the assigned data store location.
    integer(kind=c_int), intent(out) :: err
      !! Error code.

    character(4096, kind=c_char) :: data_store_buffer
    type(c_ptr) :: data_store_ptr

    interface
      function c_fdp_link_read(data_pipeline, data_product, data_store_path, length) &
          bind(C, name="fdp_link_read")
        use, intrinsic :: iso_c_binding, only: c_int, c_size_t, c_ptr
        implicit none
        type(c_ptr), intent(in), value :: data_pipeline 
        type(c_ptr), intent(in), value :: data_product
        type(c_ptr), intent(in), value :: data_store_path
        integer(kind=c_size_t), intent(in), value :: length
        integer(kind=c_int) :: c_fdp_link_read
      end function c_fdp_link_read
    end interface

    data_store_buffer = fdp_c_str_buffer(len(data_store_buffer))
    data_store_ptr = fdp_f2c_str(data_store_buffer)

    err = c_fdp_link_read(data_pipeline%ptr, &
      fdp_f2c_str(fdp_null_term(data_product)), &
      data_store_ptr, &
      int(len(data_store_buffer), kind=c_size_t) &
      )

    data_store_path = fdp_c2f_str(data_store_ptr)
  end subroutine fdp_link_read

end module fdpfort