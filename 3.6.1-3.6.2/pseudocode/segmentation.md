Segment ( input )
    segments_store := store for discovered segments
    partials := partially segmented input, initialized as deep copy of input
    try and find a segment in partials
    IF found new segment
        create segment rule and add to segment_store
        WHILE can find new segment
            replace instances of found segment in partials with segment rule symbol  
            try and find a new segment in partials
        END
    END
    finalize segmentation of remaining partials
    RETURN segmented input combined with segment_store

Find a Segment ( partials )
    data := possible locations for segment
    FOR EACH partial input IN partials
        FOR EACH contiguous sequence of signal IN the partial input
            add the signal sequence and the partial input rule's meaning to data
        END
    END

    starts := count the signs in all signals in data
    frames := initialize a search frames for the three most used signs in starts

    advance search frames until best frame is found
    RETURN found meaning and signal from best search frame
END

Initialize Search Frames (signs, data)
    initial_locations := store for initial search frames without meaning
    FOR EACH sign IN signs
        find each location in data where sign occurs
        create a search frame from each location and add to initial_locations
    END

    FOR EACH search frame IN initial_locations
        find all inputs in data where sign occurs and add them to search frame's associated inputs
    END

    initial_morphemes := store for search frames containing all possible meaning signal locations
    FOR EACH initial_frame IN initial_locations
        FOR EACH meaning IN associated inputs of initial_frame
            create new search frame with sign and meaning
            filter the associated inputs to those that contain both sign and meaning
            add new search frame to initial_morphemes
        END
    END
    RETURN initial_morphemes
END

Advance Search Frames ( search_frames )
    candidates := store for best search frames, initialized to search_frames
    beam := store for search frames to continue the search, initialized to search_frames
    WHILE still search frames in beam
        advance all search frames
        consider MDL value of advances frames and keep top 5
        update candidates with any potentially better search frames according to MDL and keep top 5
    END
    RETURN candidates
END

Advance Search Frame ( search_frame )
    IF associated input of search_frame is empty, there are no ways to extend it
        RETURN search_frame
    END

    advances := store for advanced frame
    FOR EACH associated input IN search_frame
        IF can extend the signal THEN FOR EACH extension
            create new search_frame
            update signal with extension and keep associated inputs where the updated signal occurs
            add new search frame to advances
        END
        IF can extend the meaning THE FOR EACH extension
            create new search_frame
            update meaning with extension and keep associated inputs where the updated meaning occurs
            add new search frame to advances
        END
    END
    RETURN advances
END

Finalize Segmentation of Partials ( partials )
    FOR EACH partial in partials
        IF the partial still has unsegmented meaning and signal
            create new segment to with the unsegmented meaning and signals combined
            replace occurence of new segment in partial with new segment signal
            and new segment to segment_store
        END
    END
END
