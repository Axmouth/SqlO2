import { TestBed } from '@angular/core/testing';

import { PostgrustqlService } from './postgrustql.service';

describe('PostgrustqlService', () => {
  let service: PostgrustqlService;

  beforeEach(() => {
    TestBed.configureTestingModule({});
    service = TestBed.inject(PostgrustqlService);
  });

  it('should be created', () => {
    expect(service).toBeTruthy();
  });
});
