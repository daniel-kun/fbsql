/* Copyright 2007 Daniel Albuschat
 * See license.txt for further licensing information
 */

#ifndef FBSQL_H_89327
#define FBSQL_H_89327

#ifdef FBSQL_EXPORTS
#  ifdef _MSC_VER
#     define _dllexport_ __declspec(dllexport)
#  else
#     define _dllexport_ extern
#  endif
#else
#  define _dllexport_
#endif
#ifdef _CPLUSPLUS
extern "C" {
#endif

typedef short     int16_t;
typedef int       int32_t;
typedef long long int64_t;

/* enums */

//	Transaction Access Modes
enum fbsql_TAM {amWrite, amRead};

//	Transaction Isolation Levels
enum fbsql_TIL {ilConcurrency, ilReadDirty, ilReadCommitted, ilConsistency};

//	Transaction Lock Resolution
enum fbsql_TLR {lrWait, lrNoWait};

// Transaction Table Reservation
enum fbsql_TTR {trSharedWrite, trSharedRead, trProtectedWrite, trProtectedRead};

//	Prepared Statement Types
enum fbsql_STT {stUnknown, stUnsupported,
	stSelect, stInsert, stUpdate, stDelete,	stDDL, stExecProcedure,
	stSelectUpdate, stSetGenerator, stSavePoint};

//	SQL Data Types
enum fbsql_SDT {sdArray, sdBlob, sdDate, sdTime, sdTimestamp, sdString,
	sdSmallint, sdInteger, sdLargeint, sdFloat, sdDouble};

//	Array Data Types
enum fbsql_ADT {adDate, adTime, adTimestamp, adString,
	adBool, adInt16, adInt32, adInt64, adFloat, adDouble};

// Database::Shutdown Modes
enum fbsql_DSM {dsForce, dsDenyTrans, dsDenyAttach};

// Service::StartBackup && Service::StartRestore Flags
enum fbsql_BRF {
	brVerbose = 0x1,
	// Backup flags
	brIgnoreChecksums = 0x100, brIgnoreLimbo = 0x200,
	brMetadataOnly = 0x400, brNoGarbageCollect = 0x800,
	brNonTransportable = 0x1000, brConvertExtTables = 0x2000,
	// Restore flags
	brReplace = 0x10000, brDeactivateIdx = 0x20000,
	brNoShadow = 0x40000, brNoValidity = 0x80000,
	brPerTableCommit = 0x100000, brUseAllSpace = 0x200000
};

// Service::Repair Flags
enum fbsql_RPF
{
	// Mandatory and mutually exclusives
	rpMendRecords = 0x1, rpValidatePages = 0x2, rpValidateFull = 0x4,
	// Options
	rpReadOnly = 0x100, rpIgnoreChecksums = 0x200, rpKillShadows = 0x400
};

// TransactionFactory Flags
enum fbsql_TFF {tfIgnoreLimbo = 0x1, tfAutoCommit = 0x2, tfNoAutoUndo = 0x4};

#include "database.h"
#include "transaction.h"
#include "statement.h"
#include "service.h"

_dllexport_ int      fbsql_error_occured();
_dllexport_ unsigned fbsql_error_msg_size();
_dllexport_ void     fbsql_error_msg(char *ptr);

#ifdef _CPLUSPLUS
}
#endif

#endif

