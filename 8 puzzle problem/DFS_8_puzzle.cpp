 #include<iostream>
#include<vector>
#include<stack>
#include<set>

using namespace std;

vector<vector<vector<int>>> genMove(vector<vector<int>> s){
    int row,col;
    vector<vector<vector<int>>> states;
    for(int i=0; i<3; i++){
        for(int j=0; j<3; j++){
            if(s[i][j] == 0){
                row = i;
                col = j;
                break;
            }
        }
    }
    if(row - 1 >=0){
        vector<vector<int>> temp = s;
        int tempnum  = temp[row][col];
        temp[row][col] = temp[row-1][col];
        temp[row-1][col] = tempnum;
        states.push_back(temp);
    }
    if(row + 1 <=2){
        vector<vector<int>> temp = s;
        int tempnum  = temp[row][col];
        temp[row][col] = temp[row+1][col];
        temp[row+1][col] = tempnum;
        states.push_back(temp);
    }
    if(col - 1 >=0){
        vector<vector<int>> temp = s;
        int tempnum  = temp[row][col];
        temp[row][col] = temp[row][col-1];
        temp[row][col-1] = tempnum;
        states.push_back(temp);
    }
    if(col + 1 <=2){
        vector<vector<int>> temp = s;
        int tempnum  = temp[row][col];
        temp[row][col] = temp[row][col+1];
        temp[row][col+1] = tempnum;
        states.push_back(temp);
    }
    return states;
}

string encode(vector<vector<int>> s){
    string key = "";
    for(auto &r : s){
        for(int x : r){
            key += char(x + '0');
        }
    }
    return key;
}

void solve(vector<vector<int>> initial_state){
    vector<vector<int>> goal_state = {{1,2,3},{4,5,6},{7,8,0}};
    stack<vector<vector<int>>> stk;
    set<string> visited;
    stk.push(initial_state);
    while(!stk.empty()){
        vector<vector<int>> curr = stk.top();
        stk.pop();
        visited.insert(encode(curr));
        
        if(curr == goal_state){
            cout<<"Success"<<endl;
            return;
        }
        vector<vector<vector<int>>> states = genMove(curr);
        for(auto state : states){
            string key = encode(state);
            if(!visited.count(key)){
                stk.push(state);
            }
        }
    }
    cout<<"No solution"<<endl;
}

int main(){
    vector<vector<int>> state = {{1,2,3},{4,5,6},{8,0,7}};
    solve(state);
    return 0;
}