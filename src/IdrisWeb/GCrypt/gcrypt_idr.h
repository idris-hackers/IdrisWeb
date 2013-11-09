#ifndef IDRIS_GCRYPT_H
#define IDRIS_GCRYPT_H

typedef struct idris_gcry_res {
    gcry_error_t err;
    void* res;
} idris_gcry_res;

const char* idris_gcry_init(const char* version);
gcry_error_t idris_gcry_get_struct_err(void* gcry_res_struct);
void* idris_gcry_get_struct_data(void* gcry_res_struct);
void idris_gcry_dispose(void* gcry_res_struct);
idris_gcry_res* init_res_struct();
void* idris_gcry_init_md(int algorithm_key, unsigned int flags);
gcry_error_t idris_gcry_enable_algorithm(void* context, int algorithm_key);
void idris_gcry_dispose_md(void* context);
void idris_gcry_reset_md(void* context);
char* idris_hash_string(void* context, char* str, int algorithm_key);
void idris_gcry_dispose_res(void* gcry_res_struct);




#endif
