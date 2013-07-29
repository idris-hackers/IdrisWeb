#include <gcrypt.h>
#include <gcrypt-module.h>
#include <gpg-error.h>
#include <stdbool.h>
#include <stdio.h>
#include "gcrypt_idr.h"

// LibGCrypt bindings for Idris. Provides a layer between the
// Idris code and the GCrypt code, to provide convenience functions
// and datatype marshalling.



// Initialises the libgcrypt library.
// A required version may be specified: if this is
// NULL, then GCRYPT_VERSION will be used instead.
const char* idris_gcry_init(const char* version) {
    const char* ret;
    if (version != NULL) {
        ret = gcry_check_version(version);
    } else {
        ret = gcry_check_version(GCRYPT_VERSION);
    }

    if (ret) {
        // We're ignoring secure memory for the moment,
        // although it may be good to add in future.
        gcry_control (GCRYCTL_DISABLE_SECMEM, 0);
        gcry_control (GCRYCTL_INITIALIZATION_FINISHED, 0);
    }
    return ret;
}

// Returns the gcry_error_t value from a call
gcry_error_t idris_gcry_get_struct_err(void* gcry_res_struct) {
    idris_gcry_res* struct_ptr = (idris_gcry_res*) gcry_res_struct;
    return struct_ptr->err;
}

// Returns the data associated with a call
void* idris_gcry_get_struct_data(void* gcry_res_struct) {
    idris_gcry_res* struct_ptr = (idris_gcry_res*) gcry_res_struct;
    return struct_ptr->res;
}

// Frees the result of an idris_grcy_res struct
void idris_gcry_dispose_res(void* gcry_res_struct) {
    free(gcry_res_struct);
}

// Initialises a result structure, setting the initial value of the data ptr to NULL
idris_gcry_res* init_res_struct() {
    idris_gcry_res* ret = malloc(sizeof(idris_gcry_res));
    ret->res = NULL;
    return ret;
}

// Opens a hashing context
// TODO: flags should be more idris-friendly... Maybe an array?
void* idris_gcry_init_md(int algorithm_key, unsigned int flags) {
    idris_gcry_res* result_struct = init_res_struct();
    // Allocate the context memory, perform the call
    gcry_md_hd_t* context = malloc(sizeof(gcry_md_hd_t));
    gcry_error_t err = gcry_md_open(context, algorithm_key, flags);
    // Populate the result struct, return
    result_struct->err = err;
    result_struct->res = context;
    return result_struct;
}

gcry_error_t idris_gcry_enable_algorithm(void* context, int algorithm_key) {
    // Firstly, cast back to gcry_md_hd_t*
    gcry_md_hd_t* context_ptr = (gcry_md_hd_t*) context;
    // Perform the computation, return 
    return gcry_md_enable(*context_ptr, algorithm_key);
}

void idris_gcry_dispose_md(void* context) {
    if (context != NULL) {
        gcry_md_hd_t* context_ptr = (gcry_md_hd_t*) context;
        gcry_md_close(*context_ptr);
        free(context_ptr);
    }
}

void idris_gcry_reset_md(void* context) {
    if (context != NULL) {
        gcry_md_hd_t* context_ptr = (gcry_md_hd_t*) context;
        gcry_md_reset(*context_ptr);
    }
}

// Just as a POC, I'm going to just have a string hashing function,
// as opposed to hashing arbitrary data.
// In the same vein, we return a textual representation of the digest 
// as opposed to the bitstring.
// I'll get round to doing it properly at some stage... 
char* idris_hash_string(void* context, char* str, int algorithm_key) {
    gcry_md_hd_t* context_ptr = (gcry_md_hd_t*) context;
    int str_len = strlen(str);
    // Get the buffer length required to store the digest
    unsigned int digest_length = gcry_md_get_algo_dlen(algorithm_key);
    //void* output_buffer = malloc(digest_length);
    // Put the bytes into the context 
    gcry_md_write(*context_ptr, (const void*) str, str_len);
    // Read the digest into output_buffer;
    unsigned char* output = gcry_md_read(*context_ptr, algorithm_key);
    char* str_output = malloc((digest_length * 2) + 1); // * [digest_length + 1];
    for (int i = 0; i < digest_length; i++) {
        sprintf(&str_output[i * 2], "%2.2x", output[i]);
        //printf("%2.2x", output[i]); //str_output[i]);
    }
    str_output[(digest_length * 2) + 1] = 0x00; // terminate it properly
    return str_output;
}


int main(int argc, char** argv) {
    if (argc < 2) {
        printf("Usage: thing [arg]");
        exit(0);
    }

    char* input = argv[1];
    if (idris_gcry_init(NULL)) {
        //GCRY_MD_SHA256
        void* res_struct = idris_gcry_init_md(GCRY_MD_SHA256, 0);
        void* context = ((idris_gcry_res*) res_struct)->res;
        char* output = idris_hash_string(context, input, GCRY_MD_SHA256);
        idris_gcry_dispose_md(context);
        printf("%s\n", output);
    } else {
        fprintf(stderr, "Failed to initialise");
    }
}
