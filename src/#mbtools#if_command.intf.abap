************************************************************************
* /MBTOOLS/IF_COMMAND
* MBT Command Interface
*
* (c) MBT 2020 https://marcbernardtools.com/
************************************************************************
interface /MBTOOLS/IF_COMMAND
  public .


  data COMMAND type ref to /MBTOOLS/CL_COMMAND .

  methods EXECUTE
    importing
      !I_COMMAND type STRING optional
      !I_PARAMETERS type STRING
      !I_VIA_POPUP type ABAP_BOOL optional
    returning
      value(R_EXIT) type ABAP_BOOL .
endinterface.
