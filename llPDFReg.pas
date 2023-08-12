{**************************************************}
{                                                  }
{                   llPDFLib                       }
{            Version  3.6,  09.01.2007             }
{      Copyright (c) 2002-2007  llionsoft          }
{             All rights reserved                  }
{            mailto:einfo@llion.net                }
{                                                  }
{**************************************************}

unit llPDFReg;

interface

uses
  Classes, PDF;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('llPDFLib', [TPDFDocument]);
end;

end.
 