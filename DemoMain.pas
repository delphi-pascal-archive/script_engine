unit DemoMain;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ScriptingEngine;

type
  TForm1 = class(TForm)
    Memo1: TMemo;
    Button1: TButton;
    procedure Button1Click(Sender: TObject);
  private
    FScriptEngine: TScriptEngine;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;      
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.Button1Click(Sender: TObject);
begin
  FScriptEngine.ParseScript(self, memo1.Lines);
end;

constructor TForm1.Create(AOwner: TComponent);
begin
  inherited;
  FScriptEngine := TScriptEngine.Create;
end;

destructor TForm1.Destroy;
begin
  FScriptEngine.Free;
  inherited;
end;

end.
