
#include <string>
#include "dynet/io.h"
#include "dynet/model.h"
#include "io.h"

using namespace dynet;

void init_TextFileSaver(CTextFileSaver* s, char* filename, bool append) {
    new (s) TextFileSaver(std::string(filename), append);
}

void delete_TextFileSaver(CTextFileSaver* s) {
  reinterpret_cast<TextFileSaver*>(s)->~TextFileSaver();
}

unsigned size_of_TextFileSaver() {
    return sizeof(TextFileSaver);
}


void save_ParameterCollection(CTextFileSaver* s, CModel* model, char* key) {
  reinterpret_cast<TextFileSaver*>(s)->save(*reinterpret_cast<ParameterCollection*>(model), std::string(key));
}

void save_Parameter(CTextFileSaver* s, CParameter* param, char* key) {
  reinterpret_cast<TextFileSaver*>(s)->save(*reinterpret_cast<Parameter*>(param), std::string(key));
}

void save_LookupParameter(CTextFileSaver* s, CLookupParameter* param, char* key) {
  reinterpret_cast<TextFileSaver*>(s)->save(*reinterpret_cast<LookupParameter*>(param), std::string(key));
}




void init_TextFileLoader(CTextFileLoader* l, char* filename) {
    new (l) TextFileLoader(std::string(filename));
}

void delete_TextFileLoader(CTextFileLoader* l) {
    reinterpret_cast<TextFileLoader*>(l)->~TextFileLoader();
}

unsigned size_of_TextFileLoader() {
    return sizeof(TextFileLoader);
}

void populate_ParameterCollection(CTextFileLoader* l, CModel* model, char* key) {
  reinterpret_cast<TextFileLoader*>(l)->populate(*reinterpret_cast<ParameterCollection*>(model), std::string(key));
}
void populate_Parameter(CTextFileLoader* l, CParameter* param, char* key) {
  reinterpret_cast<TextFileLoader*>(l)->populate(*reinterpret_cast<Parameter*>(param), std::string(key));
}
void populate_LookupParameter(CTextFileLoader* l, CLookupParameter* lookup_param, char* key) {
  reinterpret_cast<TextFileLoader*>(l)->populate(*reinterpret_cast<LookupParameter*>(lookup_param), std::string(key));
}

void load_param(CTextFileLoader* l, CParameter* out, CModel* model, char* key) {
  *reinterpret_cast<Parameter*>(out) =
      reinterpret_cast<TextFileLoader*>(l)->load_param(*reinterpret_cast<ParameterCollection*>(model), std::string(key));
}

void load_lookup_param(CTextFileLoader* l, CLookupParameter* out, CModel* model, char* key) {
  *reinterpret_cast<LookupParameter*>(out) =
      reinterpret_cast<TextFileLoader*>(l)->load_lookup_param(*reinterpret_cast<ParameterCollection*>(model), std::string(key));
}

