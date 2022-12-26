unit GlobalConst;

interface

const
  TmpROMExt: String = '.tmp';
  ColROMDirHeaderText: String = 'Dir';
  ColROMFileNameHeaderText: String = 'ROM(s)';
  ColOrigSizeHeaderText: String = 'Size (Kb)';
  ColEstTrimmedSizeHeaderText: String = 'Est.Size (Kb)';
  ColTrimmedSizeHeaderText: String = 'Fin.Size (Kb)';
  ColStatusHeaderText: String = 'Status';

  Byte00: Byte = $00;
  ByteFF: Byte = $FF;

  //rom errors
  reNoError: Integer = 0;
  reBadAppSize: Integer = -1;

  //trim methods
  tmAuto: Integer = -1;
  tmROMHeaderAppSize: Integer = -2;
  tmEOF: Integer = -3;

  //output folder methods
  ofSameFolderAsROM: Integer = -1;
  ofSpecifiedFolder: Integer = -2;

  //output folder name
  ofFolderName: String = 'trimmed';

  //rom processing statuses
  romStatusNone: String = '';
  romStatusAnalyse: String = 'Analysing...';
  romStatusTrim: String = 'Trimming...';
  romStatusDone: String = 'Done';
  romStatusSkip: String = 'Skipped';

  //text
  textNA: String = 'NA';
  textUnavailable: String = 'Unavailable';

  //column number
  cnumObject: Integer = 0;
  cnumStatus: Integer = 5;

  //header control hints
  hcROMFileName: String = 'ROM file name';
  hcOrigSize: String = 'Original Size (Kb)';
  hcETS: String = 'Estimated Trimmed Size (Kb)';
  hcFTS: String = 'Final Trimmed Size (Kb)';
  hcStatus: String = 'Status';
  
var
  TrimmedROMExt: String = '.x.nds';

  //string grid column
  ColROMDirWeight: Integer = 0;
  ColROMFileNameWeight: Integer = 50;
  ColOrigSizeWeight: Integer = 12;
  ColEstTrimmedSize: Integer = 12;
  ColTrimmedSizeWeight: Integer = 12;
  ColStatusWeight: Integer = 14;


implementation

end.

