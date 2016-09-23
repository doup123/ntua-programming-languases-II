
/*@predicate who_is_taller{L}(int *x,integer j, integer last)=
  @ \forall integer i; j < i < last ==> x[j] > x[i];
  @
*/

/*@ predicate how_many{L}(int *x, integer counter, integer size,integer cnt) =
  @     (counter < size) ? 
  @                      (who_is_taller(x,counter,size))? how_many(x,counter+1,size,cnt-1)
  @                                                     : how_many(x,counter+1,size,cnt)
  @                      :  cnt == 0;
  @*/


/*@ requires 0< N <= 1000000  ;
  @ requires \valid(x+(0..N-1));
  @ requires \forall integer i; 
	0 <= i <= N-1 ==> 0< x[i] <= 1000000;
  @ ensures how_many(x,0,N,\result);
  @*/
int countWhoCanSee (int N, int x[])

{

 int tallest = x[N-1];
 int count = 1;

/*@ loop invariant -1 <= i < N-1;
  @ loop invariant 1 <= count < N-i;
  @ loop invariant \exists integer j;
  @    i<j<N && tallest==x[j];
  @ loop invariant \forall integer j;
  @    i<j<N ==> tallest >=x[j];
  @ loop invariant how_many(x,i+1,N,count);      
  @ loop assigns i,count,tallest;
  @ loop variant i;
  @*/


 for (int i = N-2; i >= 0; i--)

 if (tallest < x[i]) {
 tallest = x[i];
 count++;
 }
 return count;
 }
