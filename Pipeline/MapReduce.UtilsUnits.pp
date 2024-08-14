unit MapReduce.UtilsUnits;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils;
type

    { TPattern }

    TPattern = class(TStringList)
    private
      function GetFilename(Index: integer): AnsiString;
    public
      property Filename[Index: integer]: AnsiString read GetFilename;

      constructor Create(constref StringPattern: AnsiString);
      destructor Destroy; override;

    end;

implementation

{ TPattern }

function TPattern.GetFilename(Index: integer): AnsiString;
begin

end;

constructor TPattern.Create(constref StringPattern: AnsiString);
begin

end;

destructor TPattern.Destroy;
begin
  inherited Destroy;
end;

end.

