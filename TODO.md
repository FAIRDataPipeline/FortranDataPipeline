# TODO.md

## Primary goals

- Utility module to send strings between Fortran and C
- CMake build system
- pfUnit testing
  - Will need small C lib that reads/write strings
- Interface for FDP C API
  - `init`
  - `finalise`
  - `link_read`
  - `link_write`
  - `log`
  - Error message enums
- Better `README.md`
- GitHub workflow
- New repo: FortranDataPipelineSimpleModel
  - Same SEIRS model as other languages

## Stretch goals

- Code coverage
- autoformat/linting
- fpm
