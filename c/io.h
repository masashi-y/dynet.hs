
#ifndef __INCUDE_C_IO_H
#define __INCUDE_C_IO_H

#include "dynet.h"

struct CTextFileSaver;
typedef struct CTextFileSaver CTextFileSaver;

struct CTextFileLoader;
typedef struct CTextFileLoader CTextFileLoader;


#ifdef __cplusplus
extern "C" {
#endif



void init_TextFileSaver(CTextFileSaver* s, char* filename, bool append);

void delete_TextFileSaver(CTextFileSaver* s);

unsigned size_of_TextFileSaver();

void save_ParameterCollection(CTextFileSaver* s, CModel* model, char* key);

void save_Parameter(CTextFileSaver* s, CParameter* param, char* key);

void save_LookupParameter(CTextFileSaver* s, CLookupParameter* param, char* key);




void init_TextFileLoader(CTextFileLoader* l, char* filename);

void delete_TextFileLoader(CTextFileLoader* l);

unsigned size_of_TextFileLoader();

void populate_ParameterCollection(CTextFileLoader* l, CModel* model, char* key);

void populate_Parameter(CTextFileLoader* l, CParameter* param, char* key);

void populate_LookupParameter(CTextFileLoader* l, CLookupParameter* lookup_param, char* key);

void load_param(CTextFileLoader* l, CParameter* out, CModel* model, char* key);

void load_lookup_param(CTextFileLoader* l, CLookupParameter* out, CModel* model, char* key);

#ifdef __cplusplus
}
#endif

#endif

