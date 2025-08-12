#include<iostream>
#include<vector>
#include<queue>
#include<set>
#include<map>

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

string encode(vector<vector<int>> &s){
    string key = "";
    for(auto &r : s){
        for(int x : r){
            key += char(x+'0');
        }
    }
    return key;
}

void solve(vector<vector<int>> initial_state){
    vector<vector<int>> goal_state = {{1,2,3},{4,5,6},{7,8,0}};

    queue<vector<vector<int>>> q;
    set<string> visited;

    string keyInitial = encode(initial_state);
    q.push(initial_state);
    visited.insert(keyInitial);

    while(!q.empty()){
        vector<vector<int>> curr = q.front();
        q.pop();

        if(curr == goal_state){
            cout<<"Success"<<endl;
            return;
        }

        vector<vector<vector<int>>> states = genMove(curr);
        for(auto state : states){
            string key = encode(state);
            if(!visited.count(key)){
                visited.insert(key);
                q.push(state);
            }
        }
    }
    cout<<"No solution"<<endl;
}

int main(){
    vector<vector<int>> state = {{8,1,2},{0,4,3},{7,6,5}};
    solve(state);
    return 0;
}