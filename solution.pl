% muhammed furkan atasoy
% 2017400216
% compiling: yes
% complete: yes

% artist(ArtistName, Genres, AlbumIds).
% album(AlbumId, AlbumName, ArtistNames, TrackIds).
% track(TrackId, TrackName, ArtistNames, AlbumName, [Explicit, Danceability, Energy,
%                                                    Key, Loudness, Mode, Speechiness,
%                                                    Acousticness, Instrumentalness, Liveness,
%                                                    Valence, Tempo, DurationMs, TimeSignature]).


% getArtistTracks(+ArtistName, -TrackIds, -TrackNames) 5 points
getArtistTracks(ArtistName, TrackIds, TrackNames) :-
    artist(ArtistName, _, AlbumIds),
    getAllTrackIdsFromAlbumIds(AlbumIds, TrackIds),
    getTrackNamesFromTrackIds(TrackIds, TrackNames).

% albumFeatures(+AlbumId, -AlbumFeatures) 5 points
albumFeatures(AlbumId, AlbumFeatures) :-
    album(AlbumId, _, _, TrackIds),
    getSumOfTrackFeaturesFromTrackIds(TrackIds, SumOfTrackFeatures, NumberOfTrack),
    divideList(SumOfTrackFeatures, NumberOfTrack, AlbumFeatures).

% artistFeatures(+ArtistName, -ArtistFeatures) 5 points
artistFeatures(ArtistName, ArtistFeatures) :-
    getArtistTracks(ArtistName, TrackIds, _),
    getSumOfTrackFeaturesFromTrackIds(TrackIds, SumOfTrackFeatures, NumberOfTrack),
    divideList(SumOfTrackFeatures, NumberOfTrack, ArtistFeatures).

% trackDistance(+TrackId1, +TrackId2, -Score) 5 points
trackDistance(TrackId1, TrackId2, Score) :-
    track(TrackId1, _, _, _, TrackFeature1),
    filterFeatures(TrackFeature1, FilteredTrackFeature1),
    track(TrackId2, _, _, _, TrackFeature2),
    filterFeatures(TrackFeature2, FilteredTrackFeature2),
    findDistanceOfTwoList(FilteredTrackFeature1, FilteredTrackFeature2, Score).

% albumDistance(+AlbumId1, +AlbumId2, -Score) 5 points
albumDistance(AlbumId1, AlbumId2, Score) :-
    albumFeatures(AlbumId1, AlbumFeature1),
    albumFeatures(AlbumId2, AlbumFeature2),
    findDistanceOfTwoList(AlbumFeature1, AlbumFeature2, Score).

% artistDistance(+ArtistName1, +ArtistName2, -Score) 5 points
artistDistance(ArtistName1, ArtistName2, Score) :-
    artistFeatures(ArtistName1, ArtistFeature1),
    artistFeatures(ArtistName2, ArtistFeature2),
    findDistanceOfTwoList(ArtistFeature1, ArtistFeature2, Score).

% findMostSimilarTracks(+TrackId, -SimilarIds, -SimilarNames) 10 points
findMostSimilarTracks(TrackId, SimilarIds, SimilarNames) :-
    findall(Score-Id,
            trackDistance(Id, TrackId, Score),
            TrackList),
    sort(TrackList, [_|SortedTrackList]),
    arrangeOutput(SortedTrackList, SimilarIds, SimilarNames, 0, "track").
        
% findMostSimilarAlbums(+AlbumId, -SimilarIds, -SimilarNames) 10 points
findMostSimilarAlbums(AlbumId, SimilarIds, SimilarNames) :-
    findall(Score-Id,
            albumDistance(Id, AlbumId, Score),
            AlbumList),
    sort(AlbumList, [_|SortedAlbumList]),
    arrangeOutput(SortedAlbumList, SimilarIds, SimilarNames, 0, "album").

% findMostSimilarArtists(+ArtistName, -SimilarArtists) 10 points
findMostSimilarArtists(ArtistName, SimilarArtists) :-
    findall(Score-Name,
            artistDistance(Name, ArtistName, Score),
            ArtistList),
    sort(ArtistList, [_|SortedArtistList]),
    arrangeOutput(SortedArtistList, _, SimilarArtists, 0, "artist").

% filterExplicitTracks(+TrackList, -FilteredTracks) 5 points
filterExplicitTracks([], []).
filterExplicitTracks([H|T], FilteredTracks) :-
    track(H,
          _,
          _,
          _,
          [Explicit|_]),
    filterExplicitTracks(T, FoundFilteredTracks),
    (   Explicit is 1,
        FilteredTracks=FoundFilteredTracks
    ;   Explicit is 0,
        FilteredTracks=[H|FoundFilteredTracks]
    ).

% getTrackGenre(+TrackId, -Genres) 5 points
getTrackGenre(TrackId, Genres) :-
    track(TrackId, _, ArtistNames, _, _),
    getAllGenresFromArtistNames(ArtistNames, AllGenres),
    list_to_set(AllGenres, Genres).

% discoverPlaylist(+LikedGenres, +DislikedGenres, +Features, +FileName, -Playlist) 30 points
discoverPlaylist(LikedGenres, DislikedGenres, Features, FileName, Playlist) :-
    findall(Score-Id,
            trackDistanceFromGivenFeature(Features, Id, Score),
            TrackList),
    sort(TrackList, SortedTrackList),
    arrangePlaylist(SortedTrackList, LikedGenres, DislikedGenres, Playlist, Distances, 0),
    findOtherFileInputs(Playlist, TrackNames, Artists),
    open(FileName, write, Stream),
    writeln(Stream, Playlist),
    writeln(Stream, TrackNames),
    writeln(Stream, Artists),
    writeln(Stream, Distances),
    close(Stream). 

%------------------------HELPER PREDICATES----------------------------%

% getAllTrackIdsFromAlbumIds(+AlbumIds, -AllTrackIds)
getAllTrackIdsFromAlbumIds([], []).
getAllTrackIdsFromAlbumIds([AlbumId|OtherAlbumIds], AllTrackIds) :-
    album(AlbumId, _, _, TrackIds),
    getAllTrackIdsFromAlbumIds(OtherAlbumIds, FoundTrackIds),
    append(TrackIds, FoundTrackIds, AllTrackIds).

% getTrackNamesFromTrackIds(+TrackIds, -TrackNames)
getTrackNamesFromTrackIds([], []).
getTrackNamesFromTrackIds([TrackId|OtherTrackIds], TrackNames) :-
    track(TrackId, TrackName, _, _, _),
    getTrackNamesFromTrackIds(OtherTrackIds, FoundTrackNames),
    append([TrackName], FoundTrackNames, TrackNames).

% getSumOfTrackFeaturesFromTrackIds(+TrackIds, -SumOfTrackFeatures, -NumberOfTrack)
getSumOfTrackFeaturesFromTrackIds([], [0, 0, 0, 0, 0, 0, 0, 0], 0). 
getSumOfTrackFeaturesFromTrackIds([TrackId|OtherTrackIds], SumOfTrackFeatures, NumberOfTrack) :-
    track(TrackId, _, _, _, TrackFeature),
    filterFeatures(TrackFeature, FilteredTrackFeature),
    getSumOfTrackFeaturesFromTrackIds(OtherTrackIds, FoundSumOfTrackFeatures, FoundNumberOfTrack),
    NumberOfTrack is FoundNumberOfTrack+1,
    addTwoList(FoundSumOfTrackFeatures, FilteredTrackFeature, SumOfTrackFeatures).    

% addTwoList(+List1, +List2, -ResultList)
addTwoList([], [], []).
addTwoList([H1|T1], [H2|T2], ResultList) :-
    HTotal is H1+H2,
    addTwoList(T1, T2, FoundResultList),
    append([HTotal], FoundResultList, ResultList).

% divideList(+List, +K, ResultList)
divideList([], _, []).
divideList([H|T], K, ResultList) :-
    HFinal is H/K,
    divideList(T, K, FoundResultList),
    append([HFinal], FoundResultList, ResultList).

% findDistanceOfTwoList(+List1, +List2, -Distance).
findDistanceOfTwoList(List1, List2, Distance) :-
    findDistanceOfTwoListRec(List1, List2, TotalDistance),
    Distance is sqrt(TotalDistance).

findDistanceOfTwoListRec([], [], 0).
findDistanceOfTwoListRec([H1|T1], [H2|T2], TotalDistance) :-
    HDistance is (H1-H2)**2,
    findDistanceOfTwoListRec(T1, T2, FoundTotalDistance),
    TotalDistance is FoundTotalDistance+HDistance.

% arrangeOutput(+List, -SimilarIds, -SimilarNames, +Counter, +Type)
arrangeOutput([], [], [], _, _).
arrangeOutput(_, [], [], 30, _) :-
    !.
arrangeOutput([H|T], SimilarIds, SimilarNames, Counter, "track") :-
    _-Id=H,
    track(Id, Name, _, _, _),
    FoundCounter is Counter+1,
    arrangeOutput(T, FoundSimilarIds, FoundSimilarNames, FoundCounter, "track"),
    append([Id], FoundSimilarIds, SimilarIds),
    append([Name], FoundSimilarNames, SimilarNames).

arrangeOutput([H|T], SimilarIds, SimilarNames, Counter, "album") :-
    _-Id=H,
    album(Id, Name, _, _),
    FoundCounter is Counter+1,
    arrangeOutput(T, FoundSimilarIds, FoundSimilarNames, FoundCounter, "album"),
    append([Id], FoundSimilarIds, SimilarIds),
    append([Name], FoundSimilarNames, SimilarNames).

arrangeOutput([H|T], _, SimilarNames, Counter, "artist") :-
    _-Name=H,
    FoundCounter is Counter+1,
    arrangeOutput(T, _, FoundSimilarNames, FoundCounter, "artist"),
    append([Name], FoundSimilarNames, SimilarNames).

% getAllGenresFromArtistNames(+ArtistNames, -AllGenres)
getAllGenresFromArtistNames([], []).
getAllGenresFromArtistNames([ArtistName|OtherArtistNames], AllGenres) :-
    artist(ArtistName, Genres, _),
    getAllGenresFromArtistNames(OtherArtistNames, FoundAllGenres),
    append(Genres, FoundAllGenres, AllGenres).

% trackDistanceFromGivenFeature(+Feature, +TrackId, -Score)
trackDistanceFromGivenFeature(Feature, TrackId, Score) :-
    track(TrackId, _, _, _, TrackFeature),
    filterFeatures(TrackFeature, FilteredTrackFeature),
    findDistanceOfTwoList(Feature, FilteredTrackFeature, Score).

% findOtherFileInputs(+Playlist, -TrackNames, -Artists)
findOtherFileInputs([], [], []).
findOtherFileInputs([TrackId|TailPlaylist], TrackNames, Artists) :-
    track(TrackId, TrackName, ArtistNames, _, _),
    findOtherFileInputs(TailPlaylist, FoundTrackNames, FoundArtists),
    TrackNames=[TrackName|FoundTrackNames],
    Artists=[ArtistNames|FoundArtists].

% arrangePlaylist(+List, +LikedGenres, +DislikedGenres, -ChoosenTracks, -ChoosenDistances, +Counter)
arrangePlaylist([], _, _, [], [], _).
arrangePlaylist(_, _, _, [], [], 30) :-
    !.
arrangePlaylist([H|T], LikedGenres, DislikedGenres, ChoosenTracks, ChoosenDistances, Counter) :-
    Distance-Id=H,
    getTrackGenre(Id, Genres),
    checkIntersection(LikedGenres, Genres, Flag1),
    Flag1=1,
    checkIntersection(DislikedGenres, Genres, Flag2),
    Flag2=0,
    FoundCounter is Counter+1,
    arrangePlaylist(T,
                    LikedGenres,
                    DislikedGenres,
                    FoundChoosenTracks,
                    FoundChoosenDistances,
                    FoundCounter),
    ChoosenTracks=[Id|FoundChoosenTracks],
    ChoosenDistances=[Distance|FoundChoosenDistances]. 

arrangePlaylist([_|T], LikedGenres, DislikedGenres, ChoosenTracks, ChoosenDistances, Counter) :-
    arrangePlaylist(T,
                    LikedGenres,
                    DislikedGenres,
                    ChoosenTracks,
                    ChoosenDistances,
                    Counter).

% checkIntersection(+List1, +List2, -Result)
checkIntersection([], _, 0).
checkIntersection([HeadList1|TailList1], List2, Result) :-
    checkInnerIntersection(HeadList1, List2, InnerResult),
    (   InnerResult=:=1,
        Result is 1
    ;   InnerResult=:=0,
        checkIntersection(TailList1, List2, Result)
    ).
% checkInnerIntersection(+HeadList1, +List2, -Result)
checkInnerIntersection(_, [], 0) :-
    !.
checkInnerIntersection(HeadList1, [HeadList2|_], Result) :-
    sub_string(HeadList2, _, _, _, HeadList1),
    Result is 1,
    !.
checkInnerIntersection(HeadList1, [_|TailList2], Result) :-
    checkInnerIntersection(HeadList1, TailList2, Result).

% filterFeatures(+Features, -Filtered)
features([explicit-0, danceability-1, energy-1, key-0, loudness-0, mode-1, speechiness-1, acousticness-1, instrumentalness-1, liveness-1, valence-1, tempo-0, duration_ms-0, time_signature-0]).
filterFeatures(Features, Filtered) :-
    features(X),
    filterFeaturesRec(Features, X, Filtered).
filterFeaturesRec([], [], []).
filterFeaturesRec([FeatHead|FeatTail], [Head|Tail], FilteredFeatures) :-
    filterFeaturesRec(FeatTail, Tail, FilteredTail),
    _-Use=Head,
    (   Use is 1,
        FilteredFeatures=[FeatHead|FilteredTail]
    ;   Use is 0,
        FilteredFeatures=FilteredTail
    ).
