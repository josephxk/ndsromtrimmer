unit Objects;

interface

uses
  GlobalConst, SysUtils;
type
  TROMObject = class(TObject)
    FullPath: String;
    Dir: String;
    FileName: String;
    OriginalSize: Integer;
    EstTrimmedSize: Integer;
    TrimmedSize: Integer;
    Status: String;
    TrimMethod: Integer;

  private
    { Private declarations }
  public
    { Public declarations }

  end;

type
  TOptionsObject = class(TObject)
    TrimMethod: Integer;
    OutputMethod: Integer;
    OutputFolder: String;
    KeepOriginalFileName: Boolean;
    RemoveUnderscores: Boolean;

  private
    { Private declarations }
  public
    { Public declarations }
    constructor Create;
    destructor Destroy; override;


  end;
  
implementation

constructor TOptionsObject.Create;
begin
  inherited Create;  //first call
  //init properties
  TrimMethod := tmAuto;
  OutputMethod := ofSpecifiedFolder;
  OutputFolder := IncludeTrailingBackslash(GetCurrentDir) + ofFolderName;
  KeepOriginalFileName := True;
  RemoveUnderscores := False;
end;

destructor TOptionsObject.Destroy;
begin
  inherited Destroy;  //last call
end;

end.
