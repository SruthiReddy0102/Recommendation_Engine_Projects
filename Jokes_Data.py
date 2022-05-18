# Importing Required Packages
import pandas as pd
from sklearn.metrics.pairwise import linear_kernel

#import Dataset 
Jokes = pd.read_excel("C:/Data Science/Assignments/Module-Recommendation Engine/Ratings.xlsx")
Jokes.shape #shape
Jokes.columns

# Removing Unnecessary colmuns
Jokes.drop(["id"],axis=1,inplace =True)

# creating a mapping of user_id to index number 
Jokes_index = pd.Series(Jokes.index,index=Jokes['user_id']).drop_duplicates()
Jokes_index[4]

def get_Jokes_recommendations(joke_id,topN):
    
   
    #topN = 10
    # Getting the Jokes index using its title 
    Jokes_id = Jokes_index[joke_id]
    
    # Getting the pair wise similarity score for all the jokes id's with that 
    # jokes
    cosine_scores = list(enumerate([Jokes_id]))
    
    # Sorting the cosine_similarity scores based on scores 
    cosine_scores = sorted(cosine_scores,key=lambda x:x[1],reverse = True)
    
    # Get the scores of top 10 most similar anime's 
    cosine_scores_10 = cosine_scores[0:topN+1]
    
    # Getting the Jokes index 
    Jokes_idx  =  [i[0] for i in cosine_scores_10]
    Jokes_scores =  [i[1] for i in cosine_scores_10]
    
    # Similar Joke_id and scores
    Jokes_similar_show = pd.DataFrame(columns=["joke_id","Score"])
    Jokes_similar_show["joke_id"] = Jokes.loc[Jokes_idx,"joke_id"]
    Jokes_similar_show["Score"] = Jokes_scores
    Jokes_similar_show.reset_index(inplace=True)  
    Jokes_similar_show.drop(["index"],axis=1,inplace=True)
    print (Jokes_similar_show)
    #return (Jokes_similar_show)

    
# Enter your user_id and number of joke_id to be recommended 
get_Jokes_recommendations(18,topN=3)
