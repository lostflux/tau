#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include <stdbool.h>

char** split_query(char* query);
bool isAllAlpha(char* query);

int main() {
  
  char* query = malloc(sizeof("brad  or  amanda") + 1);
  strcpy(query, "brad  or  amanda");
  int length = strlen(query);

  printf("Query: %s\n", query);

  char** words = split_query(query);

  printf("\n\nTokens: \n");

  for (int i = 0; (words[i] != NULL && i < length); i++) {
    printf("%s\n", words[i]);
  }

  for (int i = 0; i < length; i++) {
    printf("%c", query[i]);
  }

  return 0;

}


char** split_query(char* query) {

  char** words = calloc(strlen(query), sizeof(char*));

  if (words == NULL) {
    return NULL;
  }

  int index = 0;

  char* word = NULL;
  int end = strlen(query);
  for (int i = 0; i < end; i++) {

    if (word != NULL && isspace(query[i])) {
      query[i] = '\0';
      words[index++] = word;
      word = NULL;
    }

    else if (word == NULL && isalpha(query[i])) {
      word = &query[i]; 
    }
  }

  words[index++] = word;

  printf("Found %d words\n", index);
  words[index] = NULL;
  return words;
}
