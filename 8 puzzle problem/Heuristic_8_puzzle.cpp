#include<iostream>
#include<vector>
#include<queue>
#include<set>
#include<map>
#include<unordered_map>

using namespace std;

vector<vector<vector<int>>> genMove(vector<vector<int>>& s){
    int row , col;
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

string encode(vector<vector<int>>& s){
    string key = "";
    for(auto &r : s){
        for(int x : r){
            key += char(x + '0');
        }
    }
    return key;
}

int h_value(vector<vector<int>>& s){
    vector<vector<int>> goal_state = {{1,2,3},{4,5,6},{7,8,0}};
    int count = 0;
    for(int i=0; i<3; i++){
        for(int j=0; j<3; j++){
            if(s[i][j] != goal_state[i][j]){
                count++;
            }
        }
    }
    return count;
}

void solve(vector<vector<int>> s){
    struct cmp{
        bool operator()(const pair<int,string>& a, const pair<int,string>& b){
            return a.first > b.first;
        }
    };
    // Min-heap priority queue: pair<f_value, pair<g_value, state_key>>
    priority_queue<pair<int, string>, vector<pair<int, string>>, cmp> pq;
    
    unordered_map<string,vector<vector<int>>> um;
    unordered_map<string,string> parent;
    set<string> visited;

    string start_key = encode(s);
    um[start_key] = s;
    parent[start_key] = "0";
    pq.push({h_value(s),start_key});

    while(!pq.empty()){
        pair<int,string> top_element = pq.top();
        int h = top_element.first;
        string key =  top_element.second;
        pq.pop();

        if(visited.count(key)) continue;
        visited.insert(key);

        vector<vector<int>> curr = um[key];

        cout<<"h="<<h<<endl;
        for(auto& row: curr){
            for(int val : row) cout<<val<<" ";
            cout<<"\n";
        }
        cout<<"------\n";

        if(h == 0){
            cout<<"Goal reached!\n";
            return;
        }

        for(auto &next : genMove(curr)){
            string next_key = encode(next);
            if(!visited.count(next_key)){
                um[next_key] = next;
                parent[next_key] = key;
                pq.push({h_value(next),next_key});
            }
        }
    }
}

int main(){
    vector<vector<int>> initial_state = {{1,2,3},{4,0,6},{7,5,8}};
    
    cout << "Initial state:" << endl;
    for(auto& row : initial_state){
        for(int val : row){
            cout << val << " ";
        }
        cout << endl;
    }
    cout << endl;
    
    solve(initial_state);
    
    return 0;
}