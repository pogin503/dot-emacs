#include <stdio.h>
#define PRINT(fmt, x) printf("%s = " fmt,#x,(x))
#define PRINTLN(fmt, x) printf("%s = " fmt "\n",#x,(x))

#define FOR(i,a,b) for(int i=(a);i<(b);++i)
#define REP(i,n)  FOR(i,0,n)

int main(){
    int n = 0;

    scanf("%d", &n);
    printf("%d", n);

    return 0;
}
