import { BrowserModule } from '@angular/platform-browser';
import { NgModule } from '@angular/core';
import { FormsModule } from '@angular/forms';

import { AppRoutingModule } from './app-routing.module';
import { AppComponent } from './app.component';
import { ReplComponent } from './components/repl/repl.component';
import { ResultsTableComponent } from './components/results-table/results-table.component';
import { A11yModule } from '@angular/cdk/a11y';
import { TextFieldModule } from '@angular/cdk/text-field';

@NgModule({
  declarations: [AppComponent, ReplComponent, ResultsTableComponent],
  imports: [BrowserModule, AppRoutingModule, FormsModule, A11yModule, TextFieldModule],
  providers: [],
  bootstrap: [AppComponent],
})
export class AppModule {}
