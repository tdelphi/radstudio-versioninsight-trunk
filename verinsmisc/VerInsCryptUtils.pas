unit VerInsCryptUtils;

interface

 uses Windows;

 type
  HCRYPTPROV  = ULONG;
  PHCRYPTPROV = ^HCRYPTPROV;
  HCRYPTKEY   = ULONG;
  PHCRYPTKEY  = ^HCRYPTKEY;
  HCRYPTHASH  = ULONG;
  PHCRYPTHASH = ^HCRYPTHASH;

  type
    PVOID = Pointer;
    LONG  = DWORD;
    {$IFDEF UNICODE}
      LPAWSTR = PWideChar;
    {$ELSE}
      LPAWSTR = PAnsiChar;
    {$ENDIF}

  {$IFNDEF ALGIDDEF}
    {$DEFINE ALGIDDEF}
    type ALG_ID = ULONG;
  {$ENDIF}

  function CryptAcquireContext(phProv      :PHCRYPTPROV;
                              pszContainer :LPAWSTR;
                              pszProvider  :LPAWSTR;
                              dwProvType   :DWORD;
                              dwFlags      :DWORD) :BOOL;stdcall;

  function CryptCreateHash(hProv   :HCRYPTPROV;
                         Algid   :ALG_ID;
                         hKey    :HCRYPTKEY;
                         dwFlags :DWORD;
                         phHash  :PHCRYPTHASH) :BOOL;stdcall;

  function CryptHashData(hHash     :HCRYPTHASH;
                 const pbData      :PBYTE;
                       dwDataLen   :DWORD;
                       dwFlags     :DWORD) :BOOL;stdcall;

  function CryptDeriveKey(hProv     :HCRYPTPROV;
                        Algid     :ALG_ID;
                        hBaseData :HCRYPTHASH;
                        dwFlags   :DWORD;
                        phKey     :PHCRYPTKEY) :BOOL;stdcall ;

  function CryptDestroyHash(hHash :HCRYPTHASH) :BOOL;stdcall;

  function CryptEncrypt(hKey       :HCRYPTKEY;
                      hHash      :HCRYPTHASH;
                      Final      :BOOL;
                      dwFlags    :DWORD;
                      pbData     :PBYTE;
                      pdwDataLen :PDWORD;
                      dwBufLen   :DWORD) :BOOL;stdcall;

  function CryptReleaseContext(hProv   :HCRYPTPROV;
                             dwFlags :DWORD) :BOOL;stdcall;

  function CryptDecrypt(hKey       :HCRYPTKEY;
                      hHash      :HCRYPTHASH;
                      Final      :BOOL;
                      dwFlags    :DWORD;
                      pbData     :PBYTE;
                      pdwDataLen :PDWORD) :BOOL;stdcall;


function EncryptPassword(Str: string): string;
function DecryptPassword(Str: string): string;

implementation

uses SysUtils;

const
  cKEY1 = 'XMÇ27>rP&<3GF<\uKaPon)=to|/t}R3=';
  cKEY2 = 'uKaPon}=to|/t}R7=BK!32>rP[<3GF<\';
  cSALT = '~#eÑ&Yl|;../tO|}%&nÑ.}))>mÑ``+-?';


  PROV_RSA_FULL          = 1;

  // dwFlags definitions for CryptAcquireContext
  CRYPT_VERIFYCONTEXT  = $F0000000;


const
  // Algorithm classes
  ALG_CLASS_ANY          = 0;
  ALG_CLASS_SIGNATURE    = (1 shl 13);
  ALG_CLASS_MSG_ENCRYPT  = (2 shl 13);
  ALG_CLASS_DATA_ENCRYPT = (3 shl 13);
  ALG_CLASS_HASH         = (4 shl 13);
  ALG_CLASS_KEY_EXCHANGE = (5 shl 13);

  // Algorithm types
  ALG_TYPE_ANY           = 0;
  ALG_TYPE_DSS           = (1 shl 9);
  ALG_TYPE_RSA           = (2 shl 9);
  ALG_TYPE_BLOCK         = (3 shl 9);
  ALG_TYPE_STREAM        = (4 shl 9);
  ALG_TYPE_DH            = (5 shl 9);
  ALG_TYPE_SECURECHANNEL = (6 shl 9);

  // Stream cipher sub-ids
  ALG_SID_RC4            = 1;

  // Hash sub ids
  ALG_SID_SHA1           = 4;

// algorithm identifier definitions
const
  CALG_SHA1             = (ALG_CLASS_HASH or ALG_TYPE_ANY or ALG_SID_SHA1);
  CALG_RC4              = (ALG_CLASS_DATA_ENCRYPT or ALG_TYPE_STREAM or ALG_SID_RC4);


function StringToHex(S: string): string;
var
  I: Integer;
begin
  Result := '';
  for I := 1 to Length(S) do
    Result := Result + IntToHex(ord(S[I]), 2 * SizeOf(Char));
end;

function HexToString(var S: string): boolean;
var
  I: Integer;
  Str: string;
begin
  try
    Str := S;
    S := '';
    for I := 0 to Length(Str) div (2 * SizeOf(Char)) - 1 do
      S := S + Char(StrToInt('$' + copy(Str, I * 2 * SizeOf(Char) + 1, 2 * SizeOf(Char))));
    Result := True;
  except
    Result := False;
  end;
end;

function AcquireContext(var KeyHash1, KeyHash2: HCRYPTKEY): HCRYPTPROV;
var
  Hash1, Hash2: HCRYPTHASH;
  Key1, Key2: string;
begin
  CryptAcquireContext(@Result, nil, nil, PROV_RSA_FULL, CRYPT_VERIFYCONTEXT);
  CryptCreateHash(Result, CALG_SHA1, 0, 0, @Hash1);
  try
    Key1 := cKEY1;
    CryptHashData(Hash1, @Key1[1], Length(Key1) * SizeOf(Char), 0);
    CryptDeriveKey(Result, CALG_RC4, Hash1, 0, @KeyHash1);
  finally
    CryptDestroyHash(Hash1);
  end;
  CryptCreateHash(Result, CALG_SHA1, 0, 0, @Hash2);
  try
    Key2 := cKEY2;
    CryptHashData(Hash2, @Key2[1], Length(Key2) * SizeOf(Char), 0);
    CryptDeriveKey(Result, CALG_RC4, Hash2, 0, @KeyHash2);
  finally
    CryptDestroyHash(Hash2);
  end;
end;

function EncryptPassword(Str: string): string;
var
  ProvHandle: HCRYPTPROV;
  K1, K2: HCRYPTKEY;
  EResult1, EResult2: string;

  function InternalEncryptPassword(const S: string; const Key: HCRYPTKEY; out R: string): Boolean;
  var
    p:  PByte;
    sz: dword;
  begin
    if Length(S) > 0 then
    begin
      sz := Length(S) * SizeOf(Char);
      GetMem(p, sz + 8);
      try
        Move(S[1], p^, sz);
        if CryptEncrypt(Key, 0, True, 0, p, @sz, sz + 8) then
        begin
          SetLength(R, sz div SizeOf(Char));
          Move(p^, R[1], sz);
          Result := True;
        end
        else
        begin
          R := S;
          Result := False;
        end;
      finally
        FreeMem(p);
      end;
    end
    else
      Result := False;
  end;

begin
  Result := '';
  ProvHandle := AcquireContext(K1, K2);
  try
    if ProvHandle <> 0 then
      if InternalEncryptPassword(Str, K1, EResult1) then
      begin
        Insert(cSALT, EResult1, Random(Length(EResult1)) + 1);
        if InternalEncryptPassword(EResult1, K2, EResult2) then
          Result := StringToHex(EResult2);
      end;
  finally
    CryptReleaseContext(ProvHandle, 0);
  end;
end;

function DecryptPassword(Str: string): string;
var
  ProvHandle: HCRYPTPROV;
  K1, K2: HCRYPTKEY;
  EResult1, EResult2: string;

  function InternalDecryptPassword(const S: string; const Key: HCRYPTKEY; out R: string): Boolean;
  var
    p:  PByte;
    sz: dword;
  begin
    Result := False;
    R := S;
    if Length(S) > 0 then
    begin
      sz := Length(S) * SizeOf(Char);
      GetMem(p, sz);
      try
        Move(S[1], p^, sz);
        if CryptDecrypt(Key, 0, True, 0, p, @sz) then
        begin
          SetLength(R, sz div SizeOf(Char));
          Move(p^, R[1], sz);
          Result := True;
        end;
      finally
        FreeMem(p);
      end;
    end;
  end;

begin
  Result := '';
  ProvHandle := AcquireContext(K1, K2);
  try
    if ProvHandle <> 0 then
    begin
      if HexToString(Str) then
        if InternalDecryptPassword(Str, K2, EResult1) then
        begin
          EResult1 := StringReplace(EResult1, cSALT, '', [rfReplaceAll]);
          if InternalDecryptPassword(EResult1, K1, EResult2) then
            Result := EResult2;
        end;
    end;
  finally
    CryptReleaseContext(ProvHandle, 0);
  end;
end;

{$IFDEF UNICODE}
function CryptAcquireContext     ;external ADVAPI32 name 'CryptAcquireContextW';
{$ELSE}
function CryptAcquireContext     ;external ADVAPI32 name 'CryptAcquireContextA';
{$ENDIF}
function CryptCreateHash         ;external ADVAPI32 name 'CryptCreateHash';
function CryptHashData           ;external ADVAPI32 name 'CryptHashData';
function CryptDeriveKey          ;external ADVAPI32 name 'CryptDeriveKey';
function CryptDestroyHash        ;external ADVAPI32 name 'CryptDestroyHash';
function CryptEncrypt            ;external ADVAPI32 name 'CryptEncrypt';
function CryptDecrypt            ;external ADVAPI32 name 'CryptDecrypt';
function CryptReleaseContext     ;external ADVAPI32 name 'CryptReleaseContext';

end.
