unit ScriptingEngine;

interface

uses classes, sysutils, dialogs, forms;

const
  ParameterDelimiter = ',';
  ParameterQuoteChar = '"';

type
  EIncorrentSyntax = class(Exception);
  EWrongNumberOfParameters = class(Exception);
  EInvalidCommand = class(Exception);

  TScriptMethod = procedure(Sender: TObject; Params: TStrings) of object;

  TScriptCommands = class;

  TScriptEngine = class
  private
    FScriptCommands: TScriptCommands;
    procedure ParseTokens(ParamStr: string; Params: TStrings);
  public
    constructor Create;
    destructor Destroy; override;

    procedure ParseCommand(Sender: TObject; Command: string);
    procedure ParseScript(Sender: TObject; Script: TStrings);
  end;

  TScriptCommands = class
  published
    procedure ShowMessage(Sender: TObject; Params: TStrings);
    procedure Add(Sender: TObject; Params: TStrings);
    procedure CloseApp(Sender: TObject; Params: TStrings);
  end;

implementation

{ TScriptEngine }

constructor TScriptEngine.Create;
begin
  FScriptCommands := TScriptCommands.create;
end;

destructor TScriptEngine.Destroy;
begin
  FScriptCommands.free;
  inherited;
end;

procedure TScriptEngine.ParseCommand(Sender: TObject; Command: string);
var
  ScriptMethodPtr: TScriptMethod;
  Method: String;
  Parameters: TStrings;
  i, i2: integer;
begin
  if command <> '' then
  begin
    i := pos('(', command);
    i2 := pos(')', command);
    if (i < 2) or (i2 < 3) then
      raise EIncorrentSyntax.Create(
        'Command parameters must start with ( and end with )');

    method := trim(copy(command,1,i-1));

    parameters := TStringList.Create;
    try
      // fill parameters with parameters from command.
      ParseTokens(copy(command, i+1, i2-i-1), parameters);

      TMethod(ScriptMethodPtr).Data := FScriptCommands;
      TMethod(ScriptMethodPtr).Code := FScriptCommands.MethodAddress(Method);
      if assigned(ScriptMethodPtr) then
        ScriptMethodPtr(sender, parameters)
      else
        raise EInvalidCommand.CreateFmt('Command "%s" does not exist',[method]);
    finally
      parameters.Free;
    end;
  end;
end;

procedure TScriptEngine.ParseScript(Sender: TObject; Script: TStrings);
var
  i: integer;
begin
  for i := 0 to Script.Count-1 do
    ParseCommand(sender, script[i]);
end;

procedure TScriptEngine.ParseTokens(ParamStr: string; Params: TStrings);
begin
  params.QuoteChar := ParameterQuoteChar;
  params.Delimiter := ParameterDelimiter;
  params.DelimitedText := paramstr;
end;

{ TScriptCommands }

procedure TScriptCommands.Add(Sender: TObject; Params: TStrings);
var
  i: integer;
  total: integer;
begin
  total := 0;
  for i := 0 to params.Count-1 do
    inc(total, StrToInt(params[i]));
  Dialogs.ShowMessageFmt('Total: %d', [total]);
end;

procedure TScriptCommands.CloseApp(Sender: TObject; Params: TStrings);
begin
  if not (params.count in [0,1]) then
    raise EWrongNumberOfParameters.Create(
      'CloseApp expects either no parameters, or 1 parameter, '+
      'being the delay to wait before closing the application.');

  if params.Count = 1 then
    sleep(1000 * StrToInt(Params[0]));

  if sender is TForm then
    TForm(sender).Close;
end;

procedure TScriptCommands.ShowMessage(Sender: TObject; Params: TStrings);
begin
  if params.Count <> 1 then
    raise EWrongNumberOfParameters.Create(
      'ShowMessage is expecting 1 parameter: Message to display');

  Dialogs.ShowMessage(Params[0]);
end;

end.
