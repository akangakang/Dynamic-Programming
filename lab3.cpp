#include "common.h"

using namespace std;

//You should only code here.Don't edit any other files in this 
int func1(int amount, vector<int>& coins)
{
    // space O(M)
    vector<int> space0;
    vector<int> space1;

    int coinNum = coins.size();
    if(coinNum==0) return 0;

    for(int i=0;i<=amount;i++)
    {
        if(i==0)
        {
            space0.push_back(1);
            space1.push_back(1);
        } else
        {
            space0.push_back(0);
            space1.push_back(0);
        }

    }
    for(int i=1;i<coinNum;i++)
    {
        for(int a=0;a<=amount;a++)
        {
            // if i%2==0 use space0
            if(i%2==0)
            {
                if(a-coins[i-1]>=0)
                    space0[a]=space1[a]+space0[a-coins[i-1]];
                else
                    space0[a]=space1[a];
            } else{
                if(a-coins[i-1]>=0)
                    space1[a]=space0[a]+space1[a-coins[i-1]];
                else
                    space1[a]=space0[a];
            }
        }

    }

    int ans=0;
    for(int i=0;i<=amount/coins[coinNum-1];i++)
    {
        if(coinNum%2==1)
        {
            int remaining = amount - i*coins[coinNum-1];
            ans+=space0[remaining];
        } else{
            int remaining = amount - i*coins[coinNum-1];
            ans+=space1[remaining];
        }
    }
	return ans;

}

int func2(int amount, vector< vector<int> >& conquer)
{
    vector< vector<int> >space(amount+1, vector<int> (2*amount+1,0));

    for(int i=0;i<=amount;i++)
    {
        space[i][i+1]=1;
    }
	for(int n=2;n<=amount;n++)
    {
	    for(int row=1;row<=amount;row++)
        {
	        bool flag= false;
	        // to fill the space[row][row+n]
	        for(int column=row+1;column<=row+n;column++)
            {
	            int row2 = column > amount ? column - amount : column;
                int column2 = column > amount ? row+n-amount : row+n;

	            if(space[row][column]
	            &&space[row2][column2]
	            &&(conquer[row-1][column%amount-1]||(!conquer[row2-1][(column2)%amount-1])))
                {
	                flag=true;
	                space[row][row+n]=1;
                    break;
                }

            }
	        if(!flag){
                space[row][row+n]=0;
	        }
        }
    }


	// check space[row][row+amount]
	int ans=0;
	for(int row=1;row<=amount;row++)
    {
	    if(space[row][row+amount])
	        ans++;
    }

	return ans;
}

void gauss_jordan(int n,vector<vector<double>> arr,vector<double>&ans)
{
    int flag1;

    // 化简成三角
    for  (int t = 0; t < n; t++)
    {
        flag1= t;
        //because of the special condition in this problem , dont't need to swap

        //开始化简
        int flag = flag1;
        while (true){
            flag++;
            if (flag > n-1) break;
            double multiple;
            multiple = arr[flag][t] / arr[flag1][t];

            for (int j = t; j < n + 1; j++){
                arr[flag][j] -= (arr[flag1][j] * multiple);
            }

        }
    }

    ans[n-1] = arr[n-1][n] / arr[n-1][n-1];

    for (int k = n - 2; k >= 0; k--){
        double sum = 0;
        for (int j = k + 1; j <= n; j++){
            sum += (arr[k][j] * ans[j]);
        }
        ans[k] = (arr[k][n] - sum) / arr[k][k];
    }


}

void fill_noTrap(int blood,int hp,int n,const vector<int>& noTrap,const vector<int>&trap,
        const vector<int> count,const vector< vector<int> >& myEdges,const vector<int>& damage,vector< vector<double> >& space)
{
    int noTrapNum=noTrap.size();
    vector<vector<double>> arr(noTrapNum,vector<double> (noTrapNum+1,0));

    // fill the arr , preparing for gauss_jordan;
    for(int row=0;row<noTrapNum;row++)
    {
        for(int column=0;column<noTrapNum+1;column++)
        {
            if(column==row)
            {
                arr[row][column]=1;
            } else if(column==noTrapNum)
            {
                // the last column
                double sum=0;
                for(int k=0;k<trap.size();k++)
                {
                    sum+=myEdges[noTrap[row]][trap[k]]*space[blood][trap[k]]/count[trap[k]];

                }
                arr[row][column]=sum;

            } else if(column==noTrapNum-1 && column!=row)
            {
                arr[row][column]=0;
            } else{
                int hasEdge=myEdges[noTrap[row]][noTrap[column]];

                int edgeNum=count[noTrap[column]];
                if(hasEdge)
                {
                    arr[row][column]=((double)(-1)/edgeNum);
                } else
                    arr[row][column]=-0;

            }
        }
    }

    if(blood==hp)
        arr[0][noTrapNum]+=1;

    vector<double> ans(noTrapNum,0);
    gauss_jordan(noTrapNum,arr,ans);
    for(int i=0;i<noTrapNum;i++)
    {
        int node=noTrap[i];
        space[blood][node]=ans[i];
    }

}
void fill_trap(int blood,int hp,int n,const vector<int>& noTrap,const vector<int>&trap,
        const vector<int>& count,const vector< vector<int> >& myEdges,const vector<int>& damage,vector< vector<double> >& space)
{
    for(int i=0;i<trap.size();i++)
    {
        int node=trap[i];
        int last_blood=blood+damage[node];
        if(last_blood>hp)
        {
            space[blood][node]=0;
            continue;
        }
        double sum=0;
        for(int k=0;k<n-1;k++)
        {
            sum+=(myEdges[node][k]*space[last_blood][k])/count[k];
        }

        space[blood][node]=sum;

    }

}
double func3(int n,int
hp,vector<int>& damage,vector<int>& edges) {
    // row=hp+1 column=n
    vector< vector<double> >space(hp+1, vector<double> (n,0));
    vector<int> count(n,0);
    vector< vector<int> >myEdges(n, vector<int> (n,0));


    // initial edges
    for(int i=0;i+1<edges.size();i+=2)
    {
        int left=edges[i]-1;
        int right=edges[i+1]-1;
        myEdges[left][right]=1;
        myEdges[right][left]=1;
        count[left]++;
        count[right]++;
    }

    int noTrapNum=0;
    vector<int> noTrap;
    vector<int> trap;
    for(int i=0;i<n;i++)
    {
        if(!damage[i])
        {
            noTrapNum++;
            noTrap.push_back(i);
        } else{
            trap.push_back(i);
        }

    }

    if(noTrapNum==n) return 1.0;
    fill_trap(hp,hp,n,noTrap,trap,count,myEdges,damage,space);
    fill_noTrap(hp,hp,n,noTrap,trap,count,myEdges,damage,space);

    for(int row=hp-1;row>0;row--)
    {
        fill_trap(row,hp,n,noTrap,trap,count,myEdges,damage,space);
        fill_noTrap(row,hp,n,noTrap,trap,count,myEdges,damage,space);
    }
    double ans=0;
    for(int i=1;i<=hp;i++)
    {
        ans+=space[i][n-1];

    }
   // cout<<ans<<endl;
    return ans;
}